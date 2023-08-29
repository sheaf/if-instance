{-# LANGUAGE CPP #-}

module Main where

-- IfSat: tests
import Framework
  ( Test(..), runTests )
import Tests
  ( test1, test1b
  , test2, test2b
#if MIN_VERSION_ghc(9,3,0)
  , test3, test3b
#endif
  , test4, test4b
  , test5a, test5b
  )

--------------------------------------------------------------------------------

tests :: [ Test ]
tests =
  [ Test "test1"  test1  "123"
  , Test "test1b" test1b True
  , Test "test2"  test2  "<<no MyShow instance>>"
  , Test "test2b" test2b False
#if MIN_VERSION_ghc(9,3,0)
  , Test "test3"  test3  "A"
  , Test "test3b" test3b True
#endif
  , Test "test4"  test4  "<<no MyShow instance>>"
  , Test "test4b" test4b False
  , Test "test5a" test5a  'x'
  , Test "test5b" (test5b 'y') 'y'
  ]

main :: IO ()
main = runTests tests
