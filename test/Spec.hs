--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Functors                                                              --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Lab as L

--------------------------------------------------------------------------------

p1 :: L.Point Int
p1 = L.Point 4 5

p2 :: L.Point Double
p2 = L.Point 2.3 4.2

p3 :: L.Point String
p3 = L.Point "hello" "world"

instance Arbitrary a => Arbitrary (L.RoseTree a) where
    arbitrary = resize 10 $ sized arbTree
        where
            arbTree :: Int -> Gen (L.RoseTree a)
            arbTree 0 = do
                v <- arbitrary
                return (L.Leaf v)
            arbTree n = do
                Positive m <- arbitrary
                let n' = n `div` (m + 1)
                f <- replicateM m (arbTree n')
                return $ L.Node f

-- | The main entry point to the test suite.
main :: IO ()
main = hspec $ do
    describe "Functor instance for Identity" $ do
        it "fmap (+2) (Identity 4) ==> Identity 6" $
            L.fmap (+2) (L.Identity 4) `shouldBe` (L.Identity 6)
        it "fmap length (Identity \"Witter\") ==> Identity 6" $
            L.fmap length (L.Identity "Witter") `shouldBe` (L.Identity 6)
        it "fmap ($ \"cake\") (Identity length) ==> Identity 4" $
            L.fmap ($ "cake") (L.Identity length) `shouldBe` (L.Identity 4)
        it "fmap (map (=='o')) (Identity \"foo\") ==> Identity [False,True,True]" $
            L.fmap (map (=='o')) (L.Identity "foo") `shouldBe`
            (L.Identity [False,True,True])
        prop "fmap id = id" $ \(x :: Int) ->
            let p = L.Identity x in L.fmap id p === id p
        prop "fmap (f . g) = fmap f . fmap g" $ \(x :: Int) ->
            let p = L.Identity x
                f = (+1)
                g = (*2)
            in L.fmap (f . g) p === (L.fmap f . L.fmap g) p
    describe "Functor instance for Const" $ do
        it "fmap (+2) (Const 4) ==> Const 4" $
            L.fmap (+2) (L.Const 4) `shouldBe` (L.Const 4)
        it "fmap length (Const \"Witter\") ==> Const \"Witter\"" $
            L.fmap (length :: String -> Int) (L.Const "Witter") `shouldBe` (L.Const "Witter")
        it "fmap (map (=='o')) (Const \"foo\") ==> Const \"foo\"" $
            L.fmap (map (=='o')) (L.Const "foo") `shouldBe`
            (L.Const "foo")
        prop "fmap id = id" $ \(x :: Int) ->
            let p = L.Const x in L.fmap id p === id p
        prop "fmap (f . g) = fmap f . fmap g" $ \(x :: Int) ->
            let p = L.Const x
                f = (+1)
                g = (*2)
            in L.fmap (f . g) p === (L.fmap f . L.fmap g) p
    describe "Functor instance for Point" $ do
        it "fmap (+1) (Point 4 5) ==> (Point 5 6)" $
            L.fmap (+1) p1 `shouldBe` (L.Point 5 6)
        it "fmap (+1) (Point 2.3 4.2) ==> (Point 3.3 5.2)" $
            L.fmap (+1) p2 `shouldBe` (L.Point 3.3 5.2)
        it "fmap show (Point 2.3 4.2) ==> (Point \"2.3\" \"4.2\")" $
            L.fmap show p2 `shouldBe` (L.Point "2.3" "4.2")
        it "fmap length (Point \"hello\" \"world\") ==> (Point 5 5)" $
            L.fmap length p3 `shouldBe` (L.Point 5 5)
        prop "fmap id = id" $ \(x :: Int) (y :: Int) ->
            let p = L.Point x y in L.fmap id p === id p
        prop "fmap (f . g) = fmap f . fmap g" $ \(x :: Int) (y :: Int) ->
            let p = L.Point x y
                f = (+1)
                g = (*2)
            in L.fmap (f . g) p === (L.fmap f . L.fmap g) p
    describe "Functor instance for RoseTree" $ do
        it "fmap (+5) (Leaf 4) ==> Leaf 9" $
            L.fmap (+5) (L.Leaf 4) `shouldBe` (L.Leaf 9)
        it "fmap (+5) (Node []) ==> Node []" $
            L.fmap (+5) (L.Node []) `shouldBe` (L.Node [])
        it "fmap (+5) (Node [Leaf 4, Leaf 8]) ==> Node [Leaf 9, Leaf 13]" $
            L.fmap (+5) (L.Node [L.Leaf 4, L.Leaf 8])
            `shouldBe` (L.Node [L.Leaf 9, L.Leaf 13])
        it "fmap length (Node [Node [], Node [Leaf \"duck\"]])\n\t==> Node [Node [], Node [Leaf 4]]" $
            L.fmap length (L.Node [L.Node [], L.Node [L.Leaf "duck"]])
            `shouldBe` (L.Node [L.Node [], L.Node [L.Leaf 4]])
        it "fmap length (Node [Node [Leaf [1,2,3]], Node [Leaf [9,8,7]]])\n\t==> Node [Node [Leaf 6], Node [Leaf 24]]" $
            L.fmap sum (L.Node [L.Node [L.Leaf [1,2,3]], L.Node [L.Leaf [9,8,7]]])
            `shouldBe` (L.Node [L.Node [L.Leaf 6], L.Node [L.Leaf 24]])
        prop "fmap id = id" $ \(rt :: L.RoseTree Int) ->
            L.fmap id rt === id rt
        prop "fmap (f . g) = fmap f . fmap g" $ \(rt :: L.RoseTree Int) ->
            let f = (+1)
                g = (*2)
            in L.fmap (f . g) rt === (L.fmap f . L.fmap g) rt
    describe "Functor instance for Compose" $ do
        it "fmap (+5) (Compose [[1,2,3], [4,5,6]])\n\t==> Compose [[6,7,8], [9,10,11]]" $
            L.fmap (+5) (L.Compose [[1,2,3], [4,5,6]]) `shouldBe`
            L.Compose [[6,7,8], [9,10,11]]
        it "fmap not (Compose [Just True, Just False, Nothing])\n\t==> Compose [Just False, Just True, Nothing]" $
            L.fmap not (L.Compose [Just True, Just False, Nothing]) `shouldBe`
            L.Compose [Just False, Just True, Nothing]
        it "fmap even (Compose (Node [Leaf [1,2,3]]))\n\t==> Compose (Node [Leaf [False, True, False]])" $
            L.fmap even (L.Compose (L.Node [L.Leaf [1,2,3]])) `shouldBe`
            L.Compose (L.Node [L.Leaf [False, True, False]])
        it "fmap (+5) (Compose (Compose [[[1,2],[3]], [[4],[5,6]]]))\n\t==> Compose (Compose [[[6,7],[8]],[[9],[10,11]]])" $
            L.fmap (+5) (L.Compose (L.Compose [[[1,2],[3]], [[4],[5,6]]])) `shouldBe`
            L.Compose (L.Compose [[[6,7],[8]],[[9],[10,11]]])
    describe "Functor instance for State" $ do
        it "runState (fmap (*2) fresh) 4 ==> (8,5)" $
            L.runState (L.fmap (*2) L.fresh) 4 `shouldBe` (8,5)
        it "runState (fmap show fresh) 7 ==> (\"7\",8)" $
            L.runState (L.fmap show L.fresh) 7 `shouldBe` ("7",8)

--------------------------------------------------------------------------------
