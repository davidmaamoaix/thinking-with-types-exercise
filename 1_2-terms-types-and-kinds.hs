{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies #-}

import Prelude ()
import GHC.TypeLits

data Bool = True | False

type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'True y = 'True
    Or 'False y = y

-- 2.4-i
type family Not (x :: Bool) :: Bool where
    Not 'True = 'False
    Not 'False = 'True
