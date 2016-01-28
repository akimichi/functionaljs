module Chap06Spec where
 
import Test.Hspec
 
-- #@range_begin(left_and_infiniteLoop_in_haskell) 
left x y = x
infiniteLoop = infiniteLoop
-- #@range_end(left_and_infiniteLoop_in_haskell) 

-- main :: IO ()
-- main = hspec $ do
main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
  -- #@range_begin(lazy_evaluation_in_haskell) 
  describe "chap06" $ do
     it "left(1, infiniteLoop) is successful" $ do
       left 1 2 `shouldBe` 1
       left 1 infiniteLoop `shouldBe` 1  -- 無限ループに陥いらずに答えを返す
  -- #@range_end(lazy_evaluation_in_haskell) 
