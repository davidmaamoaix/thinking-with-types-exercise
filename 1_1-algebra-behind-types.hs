module AlgebraBehindTypes where

import Prelude ()

data Void -- cardinality 0
data Unit = Unit -- cardinality 1
data Bool = False | True -- cardinality 2

data Spin = Up | Down -- cardinality 2

-- isomorphism between Bool and Spin
boolToSpin1 :: Bool -> Spin
boolToSpin1 False = Up
boolToSpin1 True = Down

spinToBool1 :: Spin -> Bool
spinToBool1 Up = False
spinToBool1 Down = True

-- another one exists
boolToSpin2 :: Bool -> Spin
boolToSpin2 False = Down
boolToSpin2 True = Up

spinToBool2 :: Spin -> Bool
spinToBool2 Up = True
spinToBool2 Down = False

-- sum types
-- cardinality: |a| + |b| + |Bool| -> |a| + |b| + 2
data Deal a b = This a | That b | Other Bool

-- Maybe is a sum type of a with a unit type Nothing
-- cardinality: |a| + Unit -> |a| + 1
data Maybe a = Just a | Nothing

-- product type
-- cardinality: |a| * |b|
data Cartesian a b = Cart a b
