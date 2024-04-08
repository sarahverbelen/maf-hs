{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Property.Agreement where 

--type Agreement = [String] -- a list of the variables that have to have the same abstract value 

data Property = PReal | PInt | PBool | PAll deriving (Show, Eq)

type Agreement = [(String, Property)]