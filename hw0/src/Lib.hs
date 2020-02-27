{-# LANGUAGE TypeOperators #-}

module Lib
  ( distributivity
  , associator
  , eitherAssoc
  , doubleNeg
  , excludedNeg
  , thirdNegElim
  , s
  , composition
  , identity
  , contraction
  , permutation
  , iterateElement
  , fibonacci
  , factorial
  , mapFix
  , zero
  , succChurch
  , churchPlus
  , churchMult
  , churchToInt
  , task7_1
  , task7_2
  , task7_3
  ) where

import Data.Either (Either, lefts, rights)
import Data.Function (fix)
import Data.Maybe (mapMaybe)
import Data.Void (Void)

-- | implementation distributivity rules
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a)       = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

-- | implementation associator rules
associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

-- | implementation either associator
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (eitherAssocFirst, eitherAssocSecond)
  where
    eitherAssocFirst :: Either a (Either b c) -> Either (Either a b) c
    eitherAssocFirst (Left a)          = Left (Left a)
    eitherAssocFirst (Right (Left b))  = Left (Right b)
    eitherAssocFirst (Right (Right c)) = Right c
    eitherAssocSecond :: Either (Either a b) c -> Either a (Either b c)
    eitherAssocSecond (Left (Left a))  = Left a
    eitherAssocSecond (Left (Right b)) = Right (Left b)
    eitherAssocSecond (Right c)        = Right (Right c)

type Neg a = a -> Void

-- | hangs a double negative
doubleNeg :: a -> Neg (Neg a)
doubleNeg a f = f a

contrapositon :: (a -> b) -> (Neg b -> Neg a)
contrapositon f g = g . f

-- | excluded negative
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg f = (f . Right) (f . Left)

-- undefined
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- undefined
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | excluded double negative
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = contrapositon doubleNeg

-- | s combinator
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | implementation composition with s,k combinators
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

-- | implementation identity with s,k combinators
identity :: a -> a
identity = s const const

-- | implementation contraction with s,k combinators
contraction :: (a -> a -> b) -> a -> b
contraction = s s (const identity)

-- | implementation permutation with s,k combinators
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const s) (s (const const) s)) (const const)

-- | returns an infinite list consisting of the received element
iterateElement :: a -> [a]
iterateElement x = fix (x :)

-- | returns the N-th Fibonacci number
fibonacci :: Integer -> Integer
fibonacci =
  fix
    (\f x ->
       if x < 2
         then 1
         else f (x - 1) + f (x - 2))

-- | returns the factorial of number
factorial :: Integer -> Integer
factorial =
  fix
    (\f x ->
       if x < 2
         then 1
         else x * f (x - 1))

-- | implementation map with fix
mapFix :: (a -> b) -> [a] -> [b]
mapFix f =
  fix
    (\r xs ->
       if null xs
         then []
         else f (head xs) : r (tail xs))

type Nat a = (a -> a) -> a -> a

-- | zero church number
zero :: Nat a
zero f x = x

-- | succ church number
succChurch :: Nat a -> Nat a
succChurch n f x = f (n f x)

churchPlus, churchMult :: Nat a -> Nat a -> Nat a
-- | add two church numbers
churchPlus a b f x = a f (b f x)

-- | multiply two church numbers
churchMult a b f = a (b f)

-- | returns Integer from church number
churchToInt :: Nat Integer -> Integer
churchToInt a = a (1 +) 0

-- weak head normal form  = (Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))
-- weak head normal form = False

-- | returns False
task7_1 :: Bool
task7_1 = null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
  where
    p1 = "Dorian " :: String
    p2 = " Grey" :: String
    p3 = (++) "Dorian " :: String -> String
    p8 = (p3, p2) :: (String -> String, String)
    p4 = [(p3, p2)] :: [(String -> String, String)]
    p5 = id :: (String -> String) -> String -> String
    p6 = uncurry :: ((String -> String) -> String -> String) -> (String -> String, String) -> String
    p7 = p6 p5 :: (String -> String, String) -> String
    p9 = map :: ((String -> String, String) -> String) -> [(String -> String, String)] -> [String]
    p10 = p9 p7 :: [(String -> String, String)] -> [String]
    p11 = p10 p4 :: [String]
    p12 = ($) :: ([String] -> Bool) -> [String] -> Bool
    p13 = head :: [String] -> String
    p14 = p13 p11 :: String
    p15 = (.) :: (String -> Bool) -> ([String] -> String) -> [String] -> Bool
    p16 = null :: String -> Bool
    p17 = p16 p14 :: Bool

-- | returns (3, 64)]
task7_2 :: [(Integer, Integer)]
task7_2 = (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
  where
    p1 = 1 :: Integer
    p2 = 2 :: Integer
    p3 = 2 :: Integer
    p4 = 6 :: Integer
    p5 = (+) :: Integer -> Integer -> Integer
    p6 = p5 p1 :: Integer -> Integer
    p7 = p6 p2 :: Integer
    p8 = (^) :: Integer -> Integer -> Integer
    p9 = p8 p3 :: Integer -> Integer
    p10 = p9 p4 :: Integer
    p11 = Left :: Integer -> Either Integer Integer
    p12 = Right :: Integer -> Either Integer Integer
    p13 = p11 p7 :: Either Integer Integer
    p14 = p12 p10 :: Either Integer Integer
    p15 = [p13, p14] :: [Either Integer Integer]
    p16 = rights :: [Either Integer Integer] -> [Integer]
    p17 = lefts :: [Either Integer Integer] -> [Integer]
    p18 = zip :: [Integer] -> [Integer] -> [(Integer, Integer)]
    p19 = (\x -> p18 (lefts x) (rights x)) :: [Either Integer Integer] -> [(Integer, Integer)]
    p20 = p19 p15 :: [(Integer, Integer)]

-- | takes Integer and returns False
task7_3 :: Integer -> Bool
task7_3 =
  let impl = \x y -> not x || y
   in let isMod2 = \x -> x `mod` 2 == 0
       in let isMod4 = \x -> x `mod` 4 == 0
           in \x -> (isMod4 x) `impl` (isMod2 x)
  where
    p1 = (\x y -> not x || y) :: Bool -> Bool -> Bool
    p2 = 2 :: Integer
    p3 = 4 :: Integer
    p4 = 0 :: Integer
    p5 = (\x -> x `mod` 2 == 0) :: Integer -> Bool
    p6 = (\x -> x `mod` 4 == 0) :: Integer -> Bool
    p7 = (\x -> p5 x `p1` p6 x) :: Integer -> Bool