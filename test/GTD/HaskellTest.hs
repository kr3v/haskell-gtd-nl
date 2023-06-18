module GTD.HaskellTest where

import GTD.Haskell (haskellApplyCppHs)
import Test.Hspec (Spec, before, describe, it, shouldBe)

import System.Directory (getCurrentDirectory)

spec :: Spec
spec = do
  describe "haskellApplyCppHs" $ do
    it "applies basic C preprocessor directives" $ do
      print "inside a test"
      getCurrentDirectory >>= print
      src <- readFile "./haskellApplyCppHs/test0.in.hs"
      print src
      result <- haskellApplyCppHs "./haskellApplyCppHs/test0.in.hs" src
      print result
      result `shouldBe` "whatever"
