module Chap06Spec where
 
import Test.Hspec
 
-- #@range_begin(left_and_infiniteLoop_in_haskell) 
left x y = x
infiniteLoop () = infiniteLoop ()
-- #@range_end(left_and_infiniteLoop_in_haskell) 

-- main :: IO ()
-- main = hspec $ do
main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
  -- #@range_begin(lazy_evaluation_in_haskell) 
  describe "chap06" $ do
     it "遅延評価では無限ループに陥いらずに答えを返す" $ do
       left 1 (infiniteLoop()) `shouldBe` 1
  -- #@range_end(lazy_evaluation_in_haskell) 
