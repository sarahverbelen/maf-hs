{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Property.Agreement where 

data Property = PInt | PBool | PAll deriving (Show, Eq)
-- the property is the relevant 'field' of the modular scheme value (PAll = all fields are relevant/we don't know what field is relevant)

type Agreement = [(String, Property)] 

getVars :: Agreement -> [String]
getVars g = map fst g