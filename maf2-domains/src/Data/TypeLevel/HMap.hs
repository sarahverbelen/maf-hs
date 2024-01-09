{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE StandaloneKindSignatures   #-}

module Data.TypeLevel.HMap (
    HMap, 
    (:->),
    (::->),
    Dict(..),
    KeyType,
    KeyKind,
    HMapKey,
    withDict,
    empty, 
    singleton,
    get,
    set,
    member,
    null,
    keys,
    size, 
    map,
    filter,
    foldr,
    unionWith,
    withC,
    withC_,
    ForAll(..),
    ForAllOf,
    All,
    Assoc,
    InstanceOf,
    AtKey
) where

import Prelude hiding (map, filter, foldr, null)
import Data.Kind
import Data.Singletons
import Data.Singletons.TH

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Unsafe.Coerce (unsafeCoerce)

--
-- SomeVal (TODO: move this to other utility module?)
--

data SomeVal = forall v . SomeVal v

-- only use this when absolutely sure about the type 
unsafeCoerceVal :: SomeVal -> v
unsafeCoerceVal (SomeVal v) = unsafeCoerce v

--
-- Dict (TODO: move this to other utility module?)
--

data Dict c where
  Dict :: c => Dict c

withDict :: Dict c -> (c => v) -> v
withDict Dict = id 

---
--- Some useful constraints (TODO: move this to other utility module?)
---

data AtKey (c :: Type ~> Constraint) (m :: [k :-> Type]) :: k ~> Constraint
type instance Apply (AtKey c m) kt = c @@ Assoc kt m

data InstanceOf (c :: t -> Constraint) :: t ~> Constraint
type instance Apply (InstanceOf c) t = c t

---
--- El HMap
---

data a :-> b = a :-> b
type a ::-> b = a ':-> b -- nicer than the ':-> syntax ':-) 

type family Assoc (kt :: k) (m :: [k :-> Type]) :: Type where
  Assoc kt (kt ::-> t : r)  = t
  Assoc kt (_ : r)          = Assoc kt r
  Assoc kt '[]              = Void
type family Keys (m :: [k :-> Type]) :: [k] where
  Keys '[]              = '[]
  Keys (kt ::-> _ ': r) = (kt ': Keys r)
type family MapWith (f :: k ~> Type) (ks :: [k]) :: [k :-> Type] where
  MapWith f '[]       = '[]
  MapWith f (kt ': r) = kt ::-> (f @@ kt) : MapWith f r

type KeyKind (m :: [k :-> Type]) = k
type KeyType m = Demote (KeyKind m)

newtype HMap (m :: [k :-> Type]) = HMap (Map (KeyType m) SomeVal)

-- shorthand for mappings with a "valid"/"usable" key
type HMapKey m = (SingKind (KeyKind m), Ord (KeyType m))

empty :: HMap m
empty = HMap Map.empty

singleton :: forall m kt . (HMapKey m, SingI kt) => Assoc kt m -> HMap m
singleton = HMap . Map.singleton (demote @kt) . SomeVal

get :: forall kt m . (HMapKey m, SingI kt) => HMap m -> Maybe (Assoc kt m)
get (HMap m) = unsafeCoerceVal <$> Map.lookup (demote @kt) m

set :: forall kt m . (HMapKey m, SingI kt) => Assoc kt m -> HMap m -> HMap m
set v (HMap m) = HMap $ Map.insert (demote @kt) (SomeVal v) m

member :: forall m (kt :: KeyKind m) . (HMapKey m, SingI kt) => HMap m -> Bool
member (HMap m) = Map.member (demote @kt) m 

map :: forall f m . (HMapKey m) => (forall (kt :: KeyKind m) . Sing kt -> Assoc kt m -> f @@ kt) -> HMap m -> HMap (MapWith f (Keys m))
map f (HMap m) = HMap $ Map.mapWithKey (\k v -> withSomeSing k (g v)) m
  where g :: forall (kt :: KeyKind m) . SomeVal -> Sing kt -> SomeVal
        g v s = SomeVal $ f s (unsafeCoerceVal @(Assoc kt m) v)

filter :: forall m b . (HMapKey m) => (forall (kt :: KeyKind m) . Sing kt -> Assoc kt m -> Bool) -> HMap m -> HMap m
filter p (HMap m) = HMap $ Map.filterWithKey (\k v -> withSomeSing k (g v)) m
  where g :: forall (kt :: KeyKind m) . SomeVal -> Sing kt -> Bool
        g v s = p s (unsafeCoerceVal @(Assoc kt m) v)

foldr :: forall m b . (HMapKey m) => (forall (kt :: KeyKind m) . Sing kt -> Assoc kt m -> b -> b) -> b -> HMap m -> b
foldr f b (HMap m) = Map.foldrWithKey (\k v r -> withSomeSing k (g v r)) b m
  where g :: forall (kt :: KeyKind m) . SomeVal -> b -> Sing kt -> b
        g v r s = f s (unsafeCoerceVal @(Assoc kt m) v) r

-- TODO?: can be generalised for different mappings m1 and m2 (but might be confusing for a "union")
unionWith :: forall m b . (HMapKey m) => (forall (kt :: KeyKind m) .  Sing kt -> Assoc kt m -> Assoc kt m -> Assoc kt m) -> HMap m -> HMap m -> HMap m
unionWith f (HMap m1) (HMap m2) = HMap $ Map.unionWithKey (\k v1 v2 -> withSomeSing k (g v1 v2)) m1 m2
  where g :: forall (kt :: KeyKind m) . SomeVal -> SomeVal -> Sing kt -> SomeVal
        g v1 v2 sing = SomeVal $ f sing (unsafeCoerceVal @(Assoc kt m) v1) (unsafeCoerceVal @(Assoc kt m) v2)

keys :: HMap m -> Set (KeyType m)
keys (HMap m) = Map.keysSet m

withKey :: HMapKey m => (forall (kt :: KeyKind m) . Sing kt -> r) -> KeyType m -> r
withKey f k = withSomeSing k f 

size :: HMap m -> Int
size (HMap m) = Map.size m

null :: HMap m -> Bool
null (HMap m) = Map.null m

-- optionally, supporting these can be used for generic Eq/Semigroup/... instances
type family All k :: [k]
type ForAllOf k c = ForAllIn (All k) c
type family ForAllIn (t :: [k]) (c :: k ~> Constraint) :: Constraint where
  ForAllIn '[] c      = ()
  ForAllIn (k ': r) c = (c @@ k, ForAllIn r c)

class ForAll k c where
  for :: forall (t :: k) . Sing t -> Dict (c @@ t)

-- convenience functions combining withDict and for
withC :: forall k c t r . (ForAll k c) => (c @@ t => Sing t -> r) -> Sing t -> r
withC f s = withDict (for @k @c s) (f s)

withC_ :: forall k c t r . (ForAll k c) => (c @@ t => r) -> Sing t -> r  
withC_ f = withC @k @c (const f)

type ValueIsEq m = AtKey (InstanceOf Eq) m
instance (HMapKey m, ForAll (KeyKind m) (ValueIsEq m)) => Eq (HMap m) where
  m1 == m2 = size m1 == size m2 && all (withKey $ withC @_ @(ValueIsEq m) compareAtSing) (keys m1)
    where compareAtSing :: forall kt . ValueIsEq m @@ kt => Sing kt -> Bool
          compareAtSing Sing = get @kt m1 == get @kt m2 

type ValueIsSemigroup m = AtKey (InstanceOf Semigroup) m
instance (HMapKey m, ForAll (KeyKind m) (ValueIsSemigroup m)) => Semigroup (HMap m) where
  (<>) = unionWith (withC_ @_ @(ValueIsSemigroup m) (<>))
  

-- example 

data MyKey = IntKey | BoolKey | StringKey
  deriving (Eq, Ord)

genSingletons [''MyKey]

type instance All MyKey = [IntKey, BoolKey, StringKey]
instance ForAllOf MyKey c => ForAll MyKey c where
  for SIntKey     = Dict
  for SBoolKey    = Dict
  for SStringKey  = Dict 

myHMap :: HMap [IntKey ::-> Int, BoolKey ::-> Bool]
myHMap = set @BoolKey True $ set @IntKey 42 empty

