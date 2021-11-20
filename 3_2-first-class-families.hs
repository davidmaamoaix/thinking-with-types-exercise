{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module FirstClassFamilies where

import Prelude hiding (fst)
import Data.Kind (Constraint, Type)

fst :: (a, b) -> a
fst (a, _) = a

data Fst a b = Fst (a, b)

class Eval l t | l -> t where
    eval :: l -> t

instance Eval (Fst a b) a where
    eval (Fst (a, _)) = a

-- 10.1-i
data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
    eval (ListToMaybe []) = Nothing
    eval (ListToMaybe (x:_)) = Just x

data MapList b a = MapList (a -> b) [a]

instance (Eval b v) => Eval (MapList b a) [v] where
    eval (MapList f xs) = (eval . f) <$> xs

type Exp a = a -> Type

type family EvalT (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b

type instance EvalT (Snd '(a, b)) = b

data FromMaybe :: a -> Maybe a -> Exp a
type instance EvalT (FromMaybe _ ('Just a)) = a
type instance EvalT (FromMaybe b 'Nothing) = b

-- 10.2-i
data ListToMaybeT :: [a] -> Exp (Maybe a)
type instance EvalT (ListToMaybeT '[]) = 'Nothing
type instance EvalT (ListToMaybeT (x ': xs)) = 'Just x

data MapListT :: (a -> Exp b) -> [a] -> Exp [b]
type instance EvalT (MapListT _ '[]) = '[]
type instance EvalT (MapListT f (x ': xs)) = EvalT (f x) ': EvalT (MapListT f xs)

-- 10.2-ii
data FoldR :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance EvalT (FoldR _ z '[]) = z
type instance EvalT (FoldR f z (x ': xs)) = EvalT (f x (EvalT (FoldR f z xs)))

data Pure :: a -> Exp a
type instance EvalT (Pure x) = x

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
type instance EvalT (f =<< a) = EvalT (f (EvalT a))
infixr 0 =<<

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
type instance EvalT ((f <=< g) x) = EvalT (f (EvalT (g x)))
infixr 1 <=<

data TyEq :: a -> b -> Exp Bool
type instance EvalT (TyEq a b) = TyEqImpl a b

type family TyEqImpl (a :: k) (b :: k) :: Bool where
    TyEqImpl a a = 'True
    TyEqImpl a b = 'False

data Collapse :: [Constraint] -> Exp Constraint
type instance EvalT (Collapse '[]) = (() :: Constraint)
type instance EvalT (Collapse (x ': xs)) = (x, EvalT (Collapse xs))

type All (c :: k -> Constraint) (ts :: [k]) = Collapse =<< MapListT (Pure1 c) ts

data Pure1 :: (a -> b) -> a -> Exp b
type instance EvalT (Pure1 f x) = f x
