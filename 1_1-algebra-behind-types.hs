module AlgebraBehindTypes where

import Prelude (error, fst, snd)

data Void -- cardinality 0
data Unit = Unit -- cardinality 1
data Bool = False | True -- cardinality 2

data Spin = Up | Down -- cardinality 2

absurd :: Void -> a
absurd = error "absurd!"

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

data Either a b = Left a | Right b

-- Maybe is a sum type of a with a unit type Nothing
-- cardinality: |a| + Unit -> |a| + 1
data Maybe a = Just a | Nothing

-- product type
-- cardinality: |a| * |b|
data Cartesian a b = Cart a b

-- proving that a * 1 = a
prodUnitTo :: a -> (a, ())
prodUnitTo a = (a, ())

prodUnitFrom :: (a, ()) -> a
prodUnitFrom (a, _) = a

-- proving that a + 0 = a
sumUnitTo :: Either a Void -> a
sumUnitTo (Left a) = a
sumUnitTo (Right v) = absurd v

sumUnitFrom :: a -> Either a Void
sumUnitFrom a = Left a

-- proving that a^b * a^c = a^(b+c)
expMulTo :: ((b -> a), (c -> a)) -> (Either b c -> a)
expMulTo (f, _) (Left b) = f b
expMulTo (_, g) (Right c) = g c

-- 1.4-i
expMulFrom :: (Either b c -> a) -> ((b -> a), (c -> a))
expMulFrom f = (\b -> f (Left b), \c -> f (Right c))

-- 1.4-ii
expDisTo :: (c -> (a, b)) -> (c -> a, c -> b)
expDisTo f = (\c -> fst (f c), \c -> snd (f c))

expDisFrom :: (c -> a, c -> b) -> (c -> (a, b))
expDisFrom (f, g) = \c -> (f c, g c)
