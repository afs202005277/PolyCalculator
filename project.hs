{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char
import Data.List
import Data.Maybe (fromJust, isJust)
import Text.Read (readMaybe)

type Polynom = [Termo]

data Termo = Termo {coef :: Float, variable :: String, expo :: [Int]} deriving (Eq, Show)

{-instance Termo where
  show (c:v:e) = show (coef t) ++ "*" ++ variable t ++ "^" ++ show (expo t)-}

instance Ord Termo where
  compare t1 t2
          | variable t1 < variable t2 = LT
          | variable t1 == variable t2 && expo t1 < expo t2 = GT
          | variable t1 == variable t2 && expo t1 == expo t2 = EQ
          | otherwise = GT

normalize :: Polynom -> Polynom
normalize p = sort (filter (\x -> coef x /= 0) (sumPolynoms p))

isEqual :: Termo -> Termo -> Bool
isEqual t1 t2 = variable t1 == variable t2 && expo t1 == expo t2

grouping :: Polynom -> [Polynom]
grouping [] = []
grouping (x : xs) = filter (isEqual x) (x : xs) : grouping (filter (not . isEqual x) xs)

convertToExp :: String -> Int
convertToExp [] = 1
convertToExp (x : xs)
  | x == '+' && not (null xs) = read xs :: Int
  | x == '-' && not (null xs) = read (x : xs) :: Int
  | x == '-' && null xs = -1
  | all isDigit (x : xs) = read (x : xs) :: Int
  | otherwise = 1

findExponents :: String -> [Int]
findExponents [] = []
findExponents [a] = [1]
findExponents (x:y:xs)
            | isAlpha x && isDigit y = (read (takeWhile isDigit (y:xs)) :: Int) : findExponents xs
            | otherwise = 1 : findExponents (y:xs)

termoFactory :: String -> Termo
termoFactory str = Termo (if isJust (readMaybe (takeWhile (\x -> (not . isAlpha) x && x /= '*') str) :: Maybe Float ) then fromJust (readMaybe (takeWhile (\x -> (not . isAlpha) x && x /= '*') str)) else 1) (filter isAlpha str) (findExponents (filter (\x -> isAlpha x || isDigit x) (dropWhile (\x -> (not . isAlpha) x && x /= '*') str)))

wordSplit :: String -> Polynom
wordSplit str = [termoFactory (if signal == '+' then uterm else signal : uterm) | idx_term <- [0 .. length (words str) -1], let uterm = words str Prelude.!! idx_term; idx = if Data.Maybe.isJust (elemIndex '^' uterm) then fromJust (elemIndex '^' uterm) + 1 else 0; signal = if idx_term > 0 && head (words str Prelude.!! (idx_term -1)) == '-' then '-' else '+', uterm /= "+" && uterm /= "-"]

sumMatchingPolynoms :: Polynom -> Polynom
sumMatchingPolynoms [] = []
sumMatchingPolynoms [p] = [p]
sumMatchingPolynoms (p1 : p2 : ps) = Termo (coef p1 + coef p2) (variable p1) (expo p1) : sumMatchingPolynoms ps

sumPolynoms :: Polynom -> Polynom
sumPolynoms p = concatMap sumMatchingPolynoms (grouping p)

