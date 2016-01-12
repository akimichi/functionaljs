module Main where
 
import Test.Hspec
 
-- #@range_begin(left_and_infiniteLoop_in_haskell) 
left x y = x
infiniteLoop = infiniteLoop
-- #@range_end(left_and_infiniteLoop_in_haskell) 

main :: IO ()
main = hspec $ do
 
  describe "chap06" $ do
     it "left(1, infiniteLoop)" $ do
       -- #@range_begin(lazy_evaluation_in_haskell) 
       left 1 2 `shouldBe` 1
       left 1 infiniteLoop `shouldBe` 1 
       -- #@range_end(lazy_evaluation_in_haskell) 
