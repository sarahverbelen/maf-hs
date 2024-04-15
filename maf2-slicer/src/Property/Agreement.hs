{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Property.Agreement where 

data Property = PReal | PInt | PBool | PAll deriving (Show, Eq)
-- the property is the relevant 'field' of the modular scheme value (PAll = all fields are relevant/we don't know what field is relevant)

type Agreement = [(String, Property)] 