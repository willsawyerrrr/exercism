{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable (for_)
import Prime (nth)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "nth" $ for_ cases test
  where
    test Case {..} = it description assertion
      where
        assertion = nth (fromIntegral input) `shouldBe` expected

data Case = Case
  { description :: String,
    input :: Integer,
    expected :: Maybe Integer
  }

cases :: [Case]
cases =
  [ Case
      { description = "first prime",
        input = 1,
        expected = Just 2
      },
    Case
      { description = "second prime",
        input = 2,
        expected = Just 3
      },
    Case
      { description = "sixth prime",
        input = 6,
        expected = Just 13
      },
    Case
      { description = "big prime",
        input = 10001,
        expected = Just 104743
      },
    Case
      { description = "there is no zeroth prime",
        input = 0,
        expected = Nothing
      }
  ]
