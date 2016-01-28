module QCheckSpec where
 
import Test.Hspec
import Test.QuickCheck

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec
            
spec :: Spec
spec = do 
 describe "read" $ do
   it "is inverse to show" $ property $
     \x -> (read . show) x == (x :: Int)
