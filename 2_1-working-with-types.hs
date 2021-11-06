{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}

module WorkingWithTypes where

import Prelude (String, show, (.))
import Data.Typeable (Typeable, typeRep)

working :: forall a b. (a -> b) -> a -> b
working f a = apply
    where
        apply :: b
        apply = f a

-- phantom type parameter
data Proxy a = Proxy

typeName :: forall a. Typeable a => String
typeName = (show . typeRep) (Proxy @a)
