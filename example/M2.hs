
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=IfSat.Plugin #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module M2 where

import M1

--------------------------------------------------------------------------------

instance Show ( Bool -> Bool ) where
  show f = show [ f False, f True ]

test2 :: ( a -> a ) -> String
test2 fun = showFun fun

test3 :: ( Bool -> Bool ) -> String
test3 fun = showFun fun

test :: String
test =
  unlines
    [ test1 not
    , test2 not
    , test3 not
    , showFun not
    ]

instance C

unsafeCoerce :: Float -> String
unsafeCoerce x = foo x
