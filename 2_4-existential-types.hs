{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module ExistentialTypes where

import Data.Kind
import Data.Maybe
import Data.IORef
import Data.Typeable
import Control.Applicative
import System.IO.Unsafe (unsafePerformIO)

type Dynamic = Has Typeable

data Error = Error String

data Any where
    Any :: a -> Any

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a

data Has (c :: Type -> Constraint) where
    Has :: c t => t -> Has c

elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

instance Show (Has Show) where
    show = elimHas (("HasShow " ++) . show)

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimHas cast

-- different order than the book example
liftD2 :: forall a b r. (Typeable a, Typeable b, Typeable r) => (a -> b -> r) -> Dynamic -> Dynamic -> Maybe Dynamic
liftD2 f x y = Has <$> (f <$> (fromDynamic @a x) <*> (fromDynamic @b y))

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b = fromMaybe (Has $ Error "bad types") $ liftD2 @String @String (++) a b
                                                   <|> liftD2 @Int @Int (+) a b
                                                   <|> liftD2 @String @Int ((. show) . (++)) a b
                                                   <|> liftD2 @Int @String ((++) . show) a b

class (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a

-- ST implementation
newtype ST s a = ST { unsafeRunST :: a }

instance Functor (ST s) where
    fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
    pure = ST
    ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s) where
    ST a >>= f = seq a $ f a

newtype STRef s a = STRef { unSTRef :: IORef a }

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef = ((pure . unsafePerformIO) .) . writeIORef . unSTRef

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
    a <- readSTRef ref
    writeSTRef ref (f a)

runST :: (forall s. ST s a) -> a
runST = unsafeRunST

safeExample :: ST s String
safeExample = do
    ref <- newSTRef "Hello"
    modifySTRef ref (++ " World")
    readSTRef ref
