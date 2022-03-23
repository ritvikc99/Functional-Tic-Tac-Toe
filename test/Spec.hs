module Main where

import Control.Applicative
import Control.DeepSeq
import Data.Function
import Data.Maybe
import Data.Set.Monad (Set)
import Data.Set.Monad qualified as Set
import GHC.Generics
import Test.Hspec.Core.Spec
import Test.Hspec.Core.QuickCheck
import Test.Hspec.Core.Runner
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

import TicTacToe

deriving instance Generic (A x)
instance NFData x => NFData (A x)

deriving instance Generic (CD x)
instance NFData x => NFData (CD x)

instance E ((->) ()) where
  practice2 = ($ ())

deriving instance Generic Player
deriving via GenericArbitrary Player instance Arbitrary Player

deriving instance Generic LineIndex
deriving via GenericArbitrary LineIndex instance Arbitrary LineIndex
instance CoArbitrary LineIndex where

instance (Domain i, Eq a) => Eq (Container i a) where
  f == g = all (liftA2 (==) f g) domainList

eachHasAtLeastOnePalindromeSpec :: [[String]] -> Bool
eachHasAtLeastOnePalindromeSpec [] = True
eachHasAtLeastOnePalindromeSpec ([] : xss) = False
eachHasAtLeastOnePalindromeSpec ((x : xs) : xss) =
  eachHasAtLeastOnePalindromeSpec $
    if x == reverse x then xss else (xs : xss)

playerTokensBoardSpec :: Player -> Board (Maybe Player) -> Board [WinToken]
playerTokensBoardSpec p b i =
  let x = index i b in case i of
    (C0, C0) -> if x == Just p then [Row C0, Column C0, Backslash] else []
    (C0, C1) -> if x == Just p then [Row C0, Column C1] else []
    (C0, C2) -> if x == Just p then [Row C0, Column C2, Slash] else []
    (C1, C0) -> if x == Just p then [Row C1, Column C0] else []
    (C1, C1) -> if x == Just p then [Row C1, Column C1, Slash, Backslash] else []
    (C1, C2) -> if x == Just p then [Row C1, Column C2] else []
    (C2, C0) -> if x == Just p then [Row C2, Column C0, Slash] else []
    (C2, C1) -> if x == Just p then [Row C2, Column C1] else []
    (C2, C2) -> if x == Just p then [Row C2, Column C2, Backslash] else []

testAI :: Player -> Player -> Board (Maybe Player) -> Gen Bool
testAI ai p b =
  if checkIfWon ai b then
    pure True
  else if checkIfWon (opponent ai) b then
    pure False
  else
    case allEmptyIndices b of
      [] -> pure True
      is ->
        if p == ai then
          case findBestMove p b of
            Nothing -> pure False
            Just (i, _) ->
              testAI ai (opponent p) (update i (Just p) b)
        else do
          i <- elements is
          testAI ai (opponent p) (update i (Just p) b)

main :: IO ()
main = hspec $ do

  specify "exercise 1" $ property $ conjoin
    [ total (practice1 True)
    , total (practice2 (C True))
    , total (practice2 (D (C True) practice2))
    , total (practice3 (C True) (D (C True) practice2))
    , total (practice4 (C id) [True, False])
    , total (practice5 (\() -> (id, True)) ())
    ]

  specify "exercise 2" $ property $
    forAll arbitrary $ \(NonEmpty xs, NonEmpty ys) ->
        conjoin
          [ let arbitraryIndex zs = chooseInt (0, length zs - 1) in
              forAll (liftA2 (,) (arbitraryIndex xs) (arbitraryIndex ys)) $ \(i, j) ->
                elem (abs ((xs !! i) * (ys !! j))) (absProducts (Set.fromList xs) (Set.fromList ys))
          , forAll arbitrary $ \(x, y) ->
              (if any (\u -> any (\v -> abs (u * v) == abs (x * y)) ys) xs then elem else notElem)
                (abs (x * y)) (absProducts (Set.fromList xs) (Set.fromList ys))
          ]

  specify "exercise 3" $ property $
    forAll arbitrary $
      liftA2 (===)
        eachHasAtLeastOnePalindrome
        eachHasAtLeastOnePalindromeSpec

  specify "exercise 4" $ property $
    domainSet === Set.fromList [(False, False), (False, True), (True, False), (True, True)]

  specify "exercise 5" $ property $
    forAll arbitrary $
      liftA2 (===)
        (fmap Set.fromList . uncurry playerTokensBoard)
        (fmap Set.fromList . uncurry playerTokensBoardSpec)

  it "AI never loses" $ property $
    pendingWith "see the README for what this means"
    -- \p -> testAI p X (pure Nothing)
