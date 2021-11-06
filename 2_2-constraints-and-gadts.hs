{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ContraintsAndGADTs where

import Data.Kind (Constraint, Type)

data Expr_ a
    = (a ~ Int) => LitInt_ Int
    | (a ~ Bool) => LitBool_ Bool
    | (a ~ Int) => Add_ (Expr_ Int) (Expr_ Int)
    | (a ~ Bool) => Not_ (Expr_ Bool)
    | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)

data Expr a where
    LitInt :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Not :: Expr Bool -> Expr Bool;
    If :: Expr Bool -> Expr a -> Expr a -> Expr a

evalExpr :: Expr a -> a
evalExpr (LitInt a) = a
evalExpr (LitBool a) = a
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (Not x) = not (evalExpr x)
evalExpr (If c a b) = if evalExpr c then evalExpr a else evalExpr b

-- heterogenous list
data HList (ts :: [Type]) where
    HNil :: HList '[]
    (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

showBool :: HList '[a, Bool, b] -> String
showBool (_ :# x :# _ :# HNil) = show x

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
    All c '[] = ()
    All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
    HNil == HNil = True
    (x :# xs) == (y :# ys) = x == y && xs == ys

-- 5.3-iii
instance All Show ts => Show (HList ts) where
    show HNil = "Nil"
    show (x :# xs) = show x ++ " : " ++ show xs

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
    compare HNil HNil = EQ
    compare (x :# xs) (y :# ys) = let res = compare x y in case res of
        EQ -> compare xs ys
        otherwise -> res
