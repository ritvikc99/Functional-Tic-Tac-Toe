-- This module explores the use of the Functor, Applicative, and Foldable
-- typeclasses to work with "container" (or "collection") types like Maybe, [],
-- and Set, and reimplements the tic-tac-toe game from assignment 2 as a
-- demonstration of these concepts in application.

-- The overall goal of this assignment is to give you exercise in recognizing
-- more code patterns that can be written with standard higher-order functions,
-- this time introducing the concept of "folding" or "reducing" a collection.
-- This builds on assignment 3, where we introduced the concepts of "mapping"
-- and "filtering" collections.

module TicTacToe where

-- Ignore this set of imports for now (but feel free to ask about it if you're
-- curious). We'll introduce the imported functions as they're used.

import Control.Applicative (liftA2)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Maybe (isNothing, fromMaybe)
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MultiSet
import Data.Ord (comparing)
import Data.Set.Monad (Set)
import Data.Set.Monad qualified as Set
import Safe.Foldable (maximumByMay)

-- Remember the Functor typeclass that we introduced in lecture:
--   class Functor (f :: * -> *) where
--     fmap :: forall a b. (a -> b) -> f a -> f b

-- We have instances of Functor for most of the "container" types that we would
-- expect to be "mappable", including Maybe, [] (the list type), and Set.

-- A note on terminology: when we say something like "Set is a Functor", that
-- means "there is an instance of Functor defined for Set". This is similar to
-- how we talk about interfaces in OOP, where we might say something like
-- "HashSet is an Iterable" to mean "HashSet implements Iterable".

-- When calling fmap, we can optionally provide the container type explicitly,
-- with the same notation we've seen for explicit type applications before:
--   fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
--   fmapMaybe = fmap @Maybe

-- Here's an example using explicit type application to increment a single
-- Maybe Int value if it exists, or return Nothing if the input is Nothing.
incrementMaybeExplicit :: Maybe Int -> Maybe Int
incrementMaybeExplicit maybeX = fmap @Maybe (\x -> 1 + x) maybeX

-- As usual with explicit type applications, Haskell can figure this out from
-- context if we leave out the explicit type application.
incrementMaybe :: Maybe Int -> Maybe Int
incrementMaybe maybeX = fmap (\x -> 1 + x) maybeX

-- Most of the uses of fmap in this file will leave the container type
-- implicit, but keep in mind that it might be helpful to try to add the
-- explicit type applications yourself in order to check your understanding of
-- how the Functor typeclass works.

-- We could also simplify the definition of incrementMaybe further by using
-- partial application and operator sections, as we've covered in lecture.
incrementMaybeShorthand :: Maybe Int -> Maybe Int
incrementMaybeShorthand = fmap (1 +)

-- Most of the definitions in this file will give names to each argument, since
-- that tends to be easier to understand for beginners, but it may be good
-- practice to try to transorm some of the definitions to avoid naming their
-- arguments (sometimes known as "point-free style").


-- ================
-- EXERCISE 1 START
-- ================

-- For one last set of warm-up exercise in this course, we're going to play
-- with some higher-kinded types and typeclasses.

-- Replace each use of "undefined" with an expression of the correct type in
-- the definitions below, so that each definition passes typechecking.
-- You may add argument names to the left side of the = signs.
-- DO NOT MODIFY ANY OTHER PART OF THE DEFINITIONS.

-- You may ONLY use the following features and functions in your code for this
-- exercise: no other functions or values.
--   - lambda expressions
--   - tuple constructors (commas within parentheses)
--   - fmap, fst, snd
--   - A, C, D, E
--   - practice2

-- Each of your definitions must *terminate* for all possible inputs, meaning
-- they must not go into infinite recursion. The test suite checks for this.

data A (x :: *) where
  A :: x -> x -> A x

type B (f :: * -> *) (a :: *) = f (a, a)

practice1 :: forall x. x -> B A x
practice1 x = A (x, x) (x, x)

data CD (a :: *) where
  C :: forall a. a -> CD a
  D :: forall a. CD a -> (CD a -> a) -> CD a

class E (f :: * -> *) where
  practice2 :: forall a. f a -> a

instance E CD where
  practice2 :: forall a. CD a -> a
  practice2 a = case a of
    C a -> a
    D a f -> f a
--    a :: CD a
--    f :: CD a -> a

practice3 ::
  forall f a b. E f =>
  f a -> f b -> (a, b)
practice3 a b = (practice2 a, practice2 b)

practice4 ::
  forall f g a b. (E f, Functor g) =>
  f (a -> b) -> g a -> g b
-- practice4 f    x       y   = fmap    x       y
--           f (a -> b) (g a) = fmap (a -> b) (g a)
practice4 x y = fmap (practice2 x) y

practice5 ::
  forall f a b. Functor f =>
  f ((a -> b), a) -> f b
practice5 x = fmap (\y -> (fst y) (snd y)) x

-- ==============
-- EXERCISE 1 END
-- ==============


-- What if we want to "map" over multiple containers at once? For example, say
-- we want to add *two* Maybe Int values if they exist, and return Nothing if
-- either input is Nothing.

-- We can do this manually with pattern matching:
addMaybesByCase :: Maybe Int -> Maybe Int -> Maybe Int
addMaybesByCase (Just x) (Just y) = Just (x + y)
addMaybesByCase _ _ = Nothing

-- It seems like we should be able to implement this as a two-argument
-- "mapping" operation, like with our incrementMaybe function, but the Functor
-- typeclass actually doesn't give us enough power: fmap can only take a
-- *single* container argument as input, even if we pass it a multi-argument
-- function. This leaves us with the wrong function type:
--   (+) :: Int -> Int -> Int
--   (+) :: Int -> (Int -> Int)
--   fmap @Maybe (+) :: Maybe Int -> Maybe (Int -> Int)

-- There is a stronger typeclass called Applicative which gives us the
-- "two-argument mapping" behavior we need in the form of the liftA2 function:
--   liftA2 :: forall a b c. Applicative f => (a -> b -> c) -> f a -> f b -> f c

addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes maybeX maybeY = liftA2 (+) maybeX maybeY

-- In this naming convention, the fmap function could also be called
-- liftA1 (but it doesn't have this name anywhere in the standard library.)
--   fmap   :: forall f a b.   Functor     f => (a -> b)      -> f a -> f b
--   liftA2 :: forall f a b c. Applicative f => (a -> b -> c) -> f a -> f b -> f c

-- The Applicative typeclass also provides a liftA3 function, and in general
-- the Applicative typeclass can be used to do n-argument "mapping" for any
-- choice of n, but we won't go beyond liftA2 in this module.

-- These n-argument "mapping" functions are not in the Functor typeclass
-- because there exist data structures with implementations of fmap but not
-- liftA2/liftA3/etc., and we want to be able to give Functor instances for
-- those data structures.

-- We won't see any data structures like that in this course, though. In
-- general *most* types with a Functor instance also have an Applicative
-- instance.

-- The Applicative typeclass is actually much more powerful than what we'll see
-- here: its use extends beyond the "n-argument mapping" technique into areas
-- like parsing and form validation. For now, we'll just be thinking of it as
-- the "n-argument mapping" typeclass in the same way that Functor is the
-- "single-argument mapping" typeclass.

-- What does "n-argument mapping" mean for collections other than Maybe? Try
-- out allPairs in the REPL: it applies the two-argument (,) function to *each*
-- possible pair of elements from the two lists.
allPairs :: forall a b. [a] -> [b] -> [(a, b)]
allPairs xs ys = liftA2 (,) xs ys

-- This is the same sort of behavior we see with other collection types like
-- Set: liftA2 applies the function to *all pairs* of elements from the two
-- collections. If we wanted different behavior, there are other typeclasses
-- for other sorts of "mapping" over collections, but we'll leave that for
-- another time. (You can look up "zipWith" and "semialign" if you're curious.)

-- Note that each Applicative instance only describes *one* collection type, so
-- liftA2 can't be used to map across *two different* collection types: for
-- example, this fails to typecheck.
--   allPairsBad :: forall a b. [a] -> Set b -> [(a, b)]
--   allPairsBad xs ys = liftA2 (,) xs ys


-- ================
-- EXERCISE 2 START
-- ================

-- Replace the use of "undefined" in the absProducts function so that it
-- produces the set of absolute values of the products of each pair of elements
-- from the first and second set. You can use the built-in "abs" function to
-- get the absolute value of a number.

-- For example:
--   absProducts (Set.fromList [1, 2]) (Set.fromList [-1, 0, 1])
--     = Set.fromList
--         [ abs (1 * (-1)), abs (1 * 0), abs (1 * 1)
--         , abs (2 * (-1)), abs (2 * 0), abs (2 * 1)
--         ]
--     = Set.fromList [0, 1, 2]

-- Note that the negation operator in Haskell is a bit weird: negative numbers
-- usually need their own parentheses in expressions like "1 * (-1)".

-- DO NOT MODIFY ANY OTHER PART OF THE FUNCTION DEFINITION. Your solution must
-- not use the toList function. Your solution must not call any additional
-- top-level functions that you've defined.

absProducts :: Set Int -> Set Int -> Set Int
absProducts xs ys = liftA2 (\x y -> abs ((x) * (y))) xs ys

-- ==============
-- EXERCISE 2 END
-- ==============


-- The Functor and Applicative typeclasses capture a lot of code patterns that
-- we use when working with collection types, but certainly not all possible
-- patterns: for example, what about when we want to *consume* a collection?

-- Note that all of the methods we get from Functor and Applicative always
-- return an "f" back to us:
--   fmap   :: forall f a b.   Functor     f => (a -> b)      -> f a -> f b
--   liftA2 :: forall f a b c. Applicative f => (a -> b -> c) -> f a -> f b -> f c

-- This means that we can only *alter* the contents of the inputs, we can't
-- "reduce" them. What about something like taking the sum of a collection?
--   sum         :: [Int]     -> Int
--   Set.sum     :: Set Int   -> Int
--   fromMaybe 0 :: Maybe Int -> Int

-- Each of these "sum" functions can be implemented as a *fold* operation with
-- the foldr function that we covered in lecture.
--   sum :: forall f. Foldable f => f Int -> Int
--   sum = foldr (+) 0

-- This definition uses the Foldable typeclass, which captures patterns of
-- "folding", "reducing", "summarizing", or "aggregating" a collection. We have
-- instances of Foldable for most "container" or "collection" types, including
-- [], Set, and Maybe.

-- As we discussed in lecture, we don't actually use the foldr function
-- explicitly in our code very often. The real power of the Foldable typeclass
-- is in the variety of polymorphic functions it gives us to process collection
-- types, including but not limited to:
--   length :: forall f a. Foldable f => f a -> Int
--   toList :: forall f a. Foldable f => f a -> [a]
--   concat :: forall f a. Foldable f => f [a] -> [a]
--   sum :: forall f a. (Foldable f, Num a) => f a -> a
--   product :: forall f a. (Foldable f, Num a) => f a -> a
--   elem :: forall f a. (Foldable f, Eq a) => a -> f a -> Bool
--   all :: forall f a. Foldable f => (a -> Bool) -> f a -> Bool
--   any :: forall f a. Foldable f => (a -> Bool) -> f a -> Bool

-- Study the type signatures above carefully and experiment with the functions
-- in the REPL using lists, sets, and Maybe values to get a feel for how each
-- function works. Some examples are given below.

-- For example, let's take the sum of each set in a list of sets. This
-- definition uses explicit type application to indicate that we're using the
-- Functor instance for [] and the Foldable instance for Set.
sumOfSetsExplicit :: [Set Int] -> [Int]
sumOfSetsExplicit sets = fmap @[] (sum @Set) sets

-- Here's the same definition with implicit type applications, which is what
-- we'd usually see in real-world Haskell code.
sumOfSets :: [Set Int] -> [Int]
sumOfSets sets = fmap sum sets

-- What about the sum of all those sums? The outer sum here uses the Foldable
-- instance for [], the inner sum uses the Foldable instance for Set.
sumAllSets :: [Set Int] -> Int
sumAllSets sets = sum (fmap sum sets)

-- We can just as easily get the product of a set of lists of integers. Since
-- this code is so concise, we probably wouldn't define this sumOfProducts
-- function in a real-world codebase, we'd just use "sum (fmap product lists)"
-- directly wherever we needed it.
sumOfProducts :: Set [Int] -> Int
sumOfProducts lists = sum (fmap product lists)

-- The "all" function checks whether some Boolean function returns True for
-- *every* element in a given collection. Here we check whether every list in
-- the input set is exactly three elements long.
allInSetLength3 :: Set [Int] -> Bool
allInSetLength3 lists = all (\list -> length list == 3) lists

-- The "any" function checks whether some Boolean function returns True for
-- *at least one* element in a given collection. Here we check whether at
-- least one list in the input set is exactly three elements long.
anyInSetLength3 :: Set [Int] -> Bool
anyInSetLength3 lists = any (\list -> length list == 3) lists

-- Remember that the intuition for Maybe as a container is that it contains
-- *zero or one* values. The difference between maybeLength3 and isJustLength3
-- is how they behave on an input of Nothing.

maybeLength3 :: Maybe [Int] -> Bool
maybeLength3 maybeList = all (\list -> length list == 3) maybeList

isJustLength3 :: Maybe [Int] -> Bool
isJustLength3 maybeList = any (\list -> length list == 3) maybeList

-- All of the above examples are *monomorphic*, in that their type signatures
-- state that they act over *specific* container types. We can generalize these
-- type signatures to work for *any* Foldable type, allowing us to define new
-- functions that work for any Foldable collection.
allPositive :: forall f. Foldable f => f Int -> Bool
allPositive xs = all (\x -> x > 0) xs

-- This function checks whether all of the inner sets have the same sum.
-- For example:
--   allSetsInSetSumTo 10 (Set.fromList [Set.fromList [5, 5], Set.fromList [1, 3, 6])
--   = (5 + 5 == 10) && (1 + 3 + 6 == 10)
--   = True
--
--   allSetsInSetSumTo 10 (Set.fromList [Set.fromList [5, 5], Set.fromList [1, 2, 6])
--   = (5 + 5 == 10) && (1 + 2 + 6 == 10)
--   = False
allSetsInSetSumTo :: Int -> Set (Set Int) -> Bool
allSetsInSetSumTo total sets =
  all (\set -> sum set == total) sets

-- We can even generalize across two different Foldable types in the same
-- function: notice the similarity between allSetsInSetSumTo and allSumTo, and
-- try calling allSumTo in the REPL with a set of lists, a Maybe list, a set of
-- Maybes, a list of sets, etc. to see how it can be used on any collection
-- structure nested two layers deep.

allSumTo ::
  forall f g.
  (Foldable f, Foldable g) =>
  Int -> g (f Int) -> Bool
allSumTo total outer =
  all (\inner -> sum inner == total) outer


-- ================
-- EXERCISE 3 START
-- ================

-- Replace the use of "undefined" in the eachHasAtLeastOnePalindrome definition
-- so that it returns True if and only if every inner collection of type
-- "f String" contains at least one palindrome (a string that is the same
-- forward and in reverse). You can use the built-in "reverse" function to
-- reverse a string.

-- For example:
--   eachHasAtLeastOnePalindrome [["abcba"], ["racecar", "xyz"]]
--     = True
--   eachHasAtLeastOnePalindrome [["abcba"], ["acecar", "xyz"]]
--     = False
--   eachHasAtLeastOnePalindrome [[], ["racecar", "xyz"]]
--     = False

-- If the input list is empty, the result should be True.

-- DO NOT MODIFY ANY OTHER PART OF THE FUNCTION DEFINITION. Your solution must
-- not use the toList function. Your solution must not call any additional
-- top-level functions that you've defined.

eachHasAtLeastOnePalindrome ::
  forall f g. (Foldable f, Foldable g) =>
  g (f String) -> Bool
eachHasAtLeastOnePalindrome xss = all (\outer -> any (\inner -> (reverse inner == inner)) outer) xss

-- ==============
-- EXERCISE 3 END
-- ==============


-- Now that you've gotten a bit of exercise with Functor, Applicative, and
-- Foldable, let's put them to use in updating our tic-tac-toe code from
-- assignment 1 to be more general.

-- The main goal of this rewrite is to use higher-order functions in order to
-- reduce repetitive code in our implementation. As an added bonus, this new
-- version of the code will also be very easy to generalize to square board
-- dimensions other than 3x3, for larger variants of tic-tac-toe.

-- In this version of the code, instead of using a 3-tuple of 3-tuples for our
-- board type, we'll use a function as a general board "container". To help
-- make our function types a little more readable, we'll define a synonym that
-- lets us write function types with the name "Container" instead of an arrow.
type Container = (->)

-- If we expand out the type synonym, we can see this function's actual type:
--   index :: i -> Container i a -> a
--         :: i -> ((->) i a) -> a
--         :: i -> (i -> a) -> a
index :: forall i a. i -> Container i a -> a
index i f = f i

-- Note that there is never a need to use the "index" function above instead of
-- directly calling a Container as a function, but we will sometimes use
-- "index" to indicate that we're thinking of a function specifically as a
-- container value.

-- Similarly, the "update" function can be implemented with a lambda because
-- its type expands to have a function return type:
--   update :: i -> a -> Container i a -> Container i a
--          :: i -> a -> (i -> a) -> (i -> a)
-- We could also write the j argument on the left-hand side of the = sign,
-- taking advantage of currying.
update :: forall i a. Eq i => i -> a -> Container i a -> Container i a
update i x f =
  \j ->
    if i == j then
      x
    else
      f j

-- Conveniently, there are already built-in Functor and Applicative instances
-- for the (->) type, which we can use with our Container synonym.

-- From the Functor instance, we get single-argument mapping:
--   fmap :: forall i a b. (a -> b) -> Container i a -> Container i b

-- From the Applicative instance, we get n-argument mapping:
--   liftA2 ::
--     forall i a b c.
--     (a -> b -> c) ->
--     Container i a ->
--     Container i b ->
--     Container i c

-- There is one more Applicative method that we haven't introduced yet, which
-- is convenient with the Container type:
--   pure :: forall f a. Applicative f => a -> f a
--   pure @(Container i) :: forall a. a -> Container i a

-- The "pure" function lets us fill a container with the same value at every
-- index. To specify this, we have a law that always holds:
--   for any choice of index "i" and value "x",
--     index i (pure x) == x

-- It would be helpful to be able to print these Container values nicely, but
-- we'll need a little bit of restraint in order to make that happen.

-- Remember that it usually only makes sense to think about a function as a
-- container when the function has a *finite* input type. Not all types are
-- finite! The String type is infinite, at least in theory: any real computer
-- would eventually run out of memory trying to list every possible String, but
-- the type does not specify any upper bound on the length of a String value.

-- We introduce this Domain typeclass to categorize the set of reasonable index
-- types for a Container. If a type has a Domain instance, that means we have a
-- set that contains *every* element of that type.
class Ord a => Domain (a :: *) where
  -- We require a law to hold for every Domain instance:
  --   for any choice of value "x :: a",
  --     elem x (domainSet @a) == True
  -- This means that every possible value of type "a" must be in the domainSet
  -- specified by the "Domain a" instance. The compiler won't force us to obey
  -- this law, but we'll write tests to ensure that our instances do obey it.
  domainSet :: Set a

-- As usual, Haskell can often infer which Domain instance we want if we just
-- write "domainSet", but we can also write (for example) "domainSet @Bool" to
-- explicitly specify which Domain instance we're referring to.

-- It will sometimes be convenient to access the set of all elements in a
-- Domain type as a list instead of a Set.
domainList :: forall a. Domain a => [a]
domainList = toList (domainSet @a)

-- The Bool type is a simple Domain: it has exactly two values. You can check
-- this definition in the REPL with "domainSet @Bool".
instance Domain Bool where
  -- Proof of the Domain law:
  --   for any choice of value "x :: Bool",
  --     "x" is either "False" or "True":
  --       elem False (Set.fromList [False, True]) == True
  --       elem True  (Set.fromList [False, True]) == True
  domainSet :: Set Bool
  domainSet = Set.fromList [False, True]

-- A more subtle example is that if we have a "Domain a" instance, we can
-- create a "Domain (Maybe a)" instance. The proof of the law for this instance
-- relies on the assumption that the law already holds for the "Domain a"
-- instance: for any "y :: a", we already know that "y" must be an element of
-- "domainSet @a", by the Domain law for the "Domain a" instance.
instance forall a. Domain a => Domain (Maybe a) where
  -- Proof of the Domain law:
  --   for any choice of value "x :: Maybe a",
  --     "x" is either "Nothing" or "Just y" for some "y :: a":
  --       elem Nothing (Set.insert Nothing (fmap Just (domainSet @a)))
  --         == True
  --       elem (Just y) (Set.insert Nothing (fmap Just (domainSet @a)))
  --         == elem y (domainSet @a)
  --         == True
  domainSet :: Set (Maybe a)
  domainSet = Set.insert Nothing (fmap Just (domainSet @a))


-- ================
-- EXERCISE 4 START
-- ================

-- More relevantly to our tic-tac-toe game, a pair of Domains is also a valid
-- Domain. This is the instance that we'll be using for our BoardIndex type.

-- Replace the use of "undefined" in the domainSet definition to produce the
-- set of all possible values of type "(a, b)".

-- For example:
--   domainSet @(Bool, Maybe Bool)
--     = Set.fromList
--         [ (False, Nothing), (False, Just False), (False, Just True)
--         , (True, Nothing), (True, Just False), (True, Just True)
--         ]

-- DO NOT MODIFY ANY OTHER PART OF THE FUNCTION DEFINITION. Your solution must
-- not use the toList function. Your solution must not call any additional
-- top-level functions that you've defined.

-- You do not need to write a proof of the Domain law for your implementation,
-- but it will be checked by the automatic test suite.

-- This one should be pretty straightforward - if you're not sure where to
-- start, review the Applicative material in this module.

instance forall a b. (Domain a, Domain b) => Domain (a, b) where
  domainSet :: Set (a, b)
  domainSet = Set.fromList(allPairs (domainList @a) (domainList @b))

-- ==============
-- EXERCISE 4 END
-- ==============


-- Now that we have a way to classify what makes a reasonable index type for
-- our Container type, we can start defining ways to convert Container values
-- to strings.

-- To display a Container with a one-dimensional Domain index type, we map the
-- "show" function over the values at each index in the domain, and then use
-- the built-in "unwords" function to combine our list of Strings into a single
-- String with spaces between the elements.
showContainer1D ::
  forall i a. (Domain i, Show a) =>
  Container i a -> String
showContainer1D f =
  unwords
    (fmap
      (\i -> show (index i f))
      (domainList @i))

-- Try it out with an example Container: note that the ordering of the values
-- in the container is defined by the "Ord (Maybe Bool)" instance, which says
-- Nothing < Just True < Just False.
exampleContainer1D :: Container (Maybe Bool) Int
exampleContainer1D (Just True) = 0
exampleContainer1D (Just False) = 10
exampleContainer1D Nothing = 100

-- To display a Container with a two-dimensional Domain index type, we map the
-- "showContainer1D" function over each "sub-container" in the inner domain,
-- and then use the built-in "unlines" function to combine our list of Strings
-- into a single String with newlines between the elements.
showContainer2D ::
  forall i j a. (Domain i, Domain j, Show a) =>
  Container (i, j) a -> String
showContainer2D f =
  unlines
    (fmap
      (\i -> showContainer1D @j (\j -> f (i, j)))
      (domainList @i))

-- Try it out with an example Container.
exampleContainer2D :: Container (Bool, Bool) Int
exampleContainer2D (False, False) = 0
exampleContainer2D (False, True) = 1
exampleContainer2D (True, False) = 2
exampleContainer2D (True, True) = 3

-- A Container with a Domain index is also Foldable. You don't need to
-- understand this implementation, but it's not too deep: we get the set of
-- values at all indices and then use the existing "Foldable Set" instance to
-- do the rest of the work.
instance forall i. Domain i => Foldable (Container i) where
  foldr :: forall a b. (a -> b -> b) -> b -> Container i a -> b
  foldr f z g = foldr @Set f z (fmap g (domainSet @i))

-- With just this small Foldable instance for our Container type, we get access
-- to all the polymorphic Foldable functions:
--   length :: Domain i => Container i a -> Int
--   sum :: (Domain i, Num a) => Container i a -> a
--   all :: Domain i => (a -> Bool) -> Container i a -> Bool
--   etc.


-- Now, finally, let's rebuild our tic-tac-toe game!

-- Here is our LineIndex type, just as in assignment 2. If you modify the type
-- to have more or less constructors, it changes the dimensions of the board;
-- the only parts of this file that have to change in order to support a
-- different board size are the parts that explicitly reference the C0/C1/C2
-- constructors.
data LineIndex where
  C0 :: LineIndex
  C1 :: LineIndex
  C2 :: LineIndex
  deriving (Eq, Ord, Show)

-- The LineIndex type is a simple Domain, just like Bool but with three values
-- instead of two.
instance Domain LineIndex where
  domainSet = Set.fromList [C0, C1, C2]

-- Here is our BoardIndex type, just as in assignment 2. It has a Domain
-- instance for free, from the "Domain LineIndex" instance above and your
-- instance for tuples in exercise 4.
type BoardIndex = (LineIndex, LineIndex)

-- Here we start to differ from our assignment 2 code. Note that this
-- definition of Board has kind "* -> *": it is a *polymorphic* container,
-- unlike our assignment 2 Board type.
type Board = Container BoardIndex

-- For convenience, we give a Show instance to our Board type that uses our 2D
-- string conversion function.
instance forall a. Show a => Show (Board a) where
  show :: Board a -> String
  show = showContainer2D

-- Check out this example board in the REPL:
exampleBoard :: Board Char
exampleBoard (C0, C0) = 'A'
exampleBoard (C0, C1) = 'B'
exampleBoard (C0, C2) = 'C'
exampleBoard (C1, C0) = 'D'
exampleBoard (C1, C1) = 'E'
exampleBoard (C1, C2) = 'F'
exampleBoard (C2, C0) = 'G'
exampleBoard (C2, C1) = 'H'
exampleBoard (C2, C2) = 'I'

-- It will be convenient to have a Board that contains each index at that
-- index. Check indicesBoard in the REPL to see what this looks like.
indicesBoard :: Board BoardIndex
indicesBoard ij = ij

-- More specifically, for the actual state of our tic-tac-toe game, we will
-- have a Board of Maybe values, where Nothing indicates an empty cell.
exampleMaybeBoard :: Board (Maybe Char)
exampleMaybeBoard (C0, C0) = Just 'A'
exampleMaybeBoard (C0, C1) = Just 'B'
exampleMaybeBoard (C0, C2) = Just 'C'
exampleMaybeBoard (C1, C0) = Nothing
exampleMaybeBoard (C1, C1) = Nothing
exampleMaybeBoard (C1, C2) = Just 'F'
exampleMaybeBoard (C2, C0) = Nothing
exampleMaybeBoard (C2, C1) = Just 'H'
exampleMaybeBoard (C2, C2) = Just 'I'

-- Here's our Player type, just as in assignment 3.
data Player where
  X :: Player
  O :: Player
  deriving (Eq, Ord, Show)

opponent :: Player -> Player
opponent X = O
opponent O = X

-- To produce a list of indices that are empty on a given board, we calculate
-- the result with a pipeline of several transformations.
allEmptyIndices :: Board (Maybe a) -> [BoardIndex]
allEmptyIndices board =
  fmap fst
    (filter (\(index, element) -> isNothing element)
      (toList
        (liftA2 (,) indicesBoard
          board)))

-- Complex nested definitions like allEmptyIndices above can be hard to read,
-- so we have an alternate way to write them that flows a little more
-- naturally. The (&) operator takes a function on the *right* and an argument
-- on the *left* and applies the function to the argument. This definition has
-- the exact same meaning as the one above.
allEmptyIndicesFlipped :: Board (Maybe a) -> [BoardIndex]
allEmptyIndicesFlipped board =
  board
    & liftA2 (,) indicesBoard
    & toList
    & filter (\(index, element) -> isNothing element)
    & fmap fst

-- How do we know when a player has won a game of tic-tac-toe?

-- There are eight different ways to win: three horizontal rows, three vertical
-- columns, and two diagonal lines. Let's represent those in a type. The Slash
-- and Backslash constructors represent the diagonals: '/' and '\'.
data WinToken where
  Row :: LineIndex -> WinToken
  Column :: LineIndex -> WinToken
  Slash :: WinToken
  Backslash :: WinToken
  deriving (Eq, Ord, Show)

-- Note that the WinToken type is valid even if we add more constructors to our
-- LineIndex type: it generalizes to different square board sizes "for free".

-- The mirrorIndex function takes the "inverse" of a LineIndex; we'll use it
-- below to describe a diagonal line.
mirrorIndex :: LineIndex -> LineIndex
mirrorIndex C0 = C2
mirrorIndex C1 = C1
mirrorIndex C2 = C0

-- This Board represents every possible way to win a game of tic-tac-toe: each
-- cell contains a list of "tokens" that a player collects for moving in that
-- cell, and a player can win by collecting three of the same token. Check
-- tokensBoard in the REPL to see what it looks like.
tokensBoard :: Board [WinToken]
tokensBoard (i, j) =
  [Row i, Column j] ++
    (if i == j then [Backslash] else []) ++
    (if i == mirrorIndex j then [Slash] else [])

-- For example, consider this game board, where '.' is an empty space:

--   X O O
--   X X X
--   O O .

-- X's tokens are
--   [ Row C0, Column C0, Backslash        -- from upper-left
--   , Row C1, Column C0                   -- from center-left
--   , Row C1, Column C1, Backslash, Slash -- from center
--   , Row C1, Column C2                   -- from center-right
--   ]

-- X has won because they have three "Row C1" tokens.


-- ================
-- EXERCISE 5 START
-- ================

-- Replace the use of "undefined" in the playerTokensBoard definition so that
-- it produces a board containing only the WinTokens that a player has
-- collected in the current game.

-- For example, consider the game board from above again:

--   X O O
--   X X X
--   O O .

-- The output of playerTokensBoard for X on this board should look like:
--   [Row C0, Column C0, Backslash] [] []
--   [Row C1, Column C0] [Row C1, Column C1, Backslash, Slash] [Row C1, Column C2]
--   [] [] []

-- The output of playerTokensBoard for O on this board should look like:
--   [] [Row C0, Column C1] [Row C0, Column C2, Slash]
--   [] [] []
--   [Row C2, Column C0, Slash] [Row C2, Column C1] []

-- DO NOT MODIFY ANY OTHER PART OF THE FUNCTION DEFINITION. Your solution must
-- not use the toList function. Your solution must not call any additional
-- top-level functions that you've defined. Your solution must not explicitly
-- use the C0/C1/C2 constructors.

playerTokensBoard :: Player -> Board (Maybe Player) -> Board [WinToken]
playerTokensBoard p b = liftA2 (\maybePlayer tokens -> case maybePlayer of
  Just p2 -> if p2 == p then tokens else []
  Nothing -> []) b tokensBoard 

-- ==============
-- EXERCISE 5 END
-- ==============


-- With your solution to exercise 5, we can produce the MultiSet of WinToken
-- values that a player has collected on a given board: first we use "concat"
-- to join the Board of lists into a single list, and then we convert the list
-- to a MultiSet.
playerTokens :: Player -> Board (Maybe Player) -> MultiSet WinToken
playerTokens p board = MultiSet.fromList (concat (playerTokensBoard p board))

-- To check if a player has won the game, we check if there is any token with
-- three occurrences in their MultiSet of tokens.
checkIfWon :: Player -> Board (Maybe Player) -> Bool
checkIfWon p board =
  let tokens = playerTokens p board in
    any (\t -> MultiSet.occur t tokens == 3) tokens

-- Our AI's rateMoveOutcome function looks almost identical to before.
rateMoveOutcome :: BoardIndex -> Player -> Board (Maybe Player) -> Int
rateMoveOutcome i p b =
  let b1 = update i (Just p) b in
    if checkIfWon p b1 then
      1
    else if checkIfWon (opponent p) b1 then
      -1
    else
      case findBestMove (opponent p) b1 of
        Nothing -> 0
        Just (_, x) -> negate x

-- Finally, to find a player's best move, we get the list of empty indices on
-- the board, pair each empty index with its rating, and calculate the maximum
-- move by comparing each tuple in the list on its second value. There are a
-- couple functions used in here that we haven't seen before: check their types
-- and try them out in the REPL for yourself, and feel free to ask if you have
-- questions about how they work!
findBestMove :: Player -> Board (Maybe Player) -> Maybe (BoardIndex, Int)
findBestMove p board =
  board
    & allEmptyIndices
    & fmap (\i -> (i, rateMoveOutcome i p board))
    & maximumByMay (comparing snd)
