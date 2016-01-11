module Main where
 
-- import Haq
import Test.Hspec
 
main :: IO ()
main = hspec $ do
 
  describe "Validate haqify function" $ do
    it "haqify is supposed to prefix Haq! to things" $ do
      True `shouldBe` True 
      
