{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char
import Data.List
import Data.Maybe
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
normalize p = reverse (sort (filter (\x -> coef x /= 0) (sumPolynoms p)))

isEqual :: Termo -> Termo -> Bool
isEqual t1 t2 = variable t1 == variable t2 && expo t1 == expo t2

grouping :: Polynom -> [Polynom]
grouping [] = []
grouping (x : xs) = filter (isEqual x) (x : xs) : grouping (filter (not . isEqual x) xs)

findExponents :: String -> [Int]
findExponents [] = []
findExponents [a] = [1]
findExponents (x : y : xs)
  | isAlpha x && isDigit y = (read (takeWhile isDigit (y : xs)) :: Int) : findExponents xs
  | otherwise = 1 : findExponents (y : xs)

termoFactory :: String -> Termo
termoFactory str = Termo coefficient (filter isAlpha str) (findExponents (filter (\x -> isAlpha x || isDigit x) (dropWhile (\x -> (not . isAlpha) x && x /= '*') str)))
  where
    coefficient = fromMaybe 1 attempt
    attempt = readMaybe (takeWhile (\x -> (not . isAlpha) x && x /= '*') str) :: Maybe Float

wordSplit :: String -> Polynom
wordSplit str =
  [ termoFactory (if signal == '+' then uterm else signal : uterm) | idx_term <- [0 .. length (words str) -1], let uterm = words str !! idx_term
                                                                                                                   signal = if idx_term > 0 && head (words str !! (idx_term -1)) == '-' then '-' else '+', uterm /= "+" && uterm /= "-"
  ]

sumMatchingPolynoms :: Polynom -> Polynom
sumMatchingPolynoms [] = []
sumMatchingPolynoms [p] = [p]
sumMatchingPolynoms (p1 : p2 : ps) = Termo (coef p1 + coef p2) (variable p1) (expo p1) : sumMatchingPolynoms ps

sumPolynoms :: Polynom -> Polynom
sumPolynoms p = concatMap sumMatchingPolynoms (grouping p)

expandExponents :: String -> [Int] -> String
expandExponents [] _ = []
expandExponents _ [] = []
expandExponents (x:xs) (n:ns) = replicate n x ++ expandExponents xs ns

collapseExponents :: String -> [Int]
collapseExponents [] = []
collapseExponents (x:xs) = length (filter (x==) (x:xs)) : collapseExponents (filter (x/=) xs)

multiplyTerms :: Termo -> Termo -> Termo
multiplyTerms t1 t2 = Termo (coef t1 * coef t2) (nub variables) (collapseExponents variables)
                      where variables = expandExponents (variable t1) (expo t1) ++ expandExponents (variable t2) (expo t2)

multiplyPolynoms :: Polynom -> Polynom -> Polynom
multiplyPolynoms p1 p2 = [multiplyTerms t1 t2 | t1 <- p1, t2 <- p2]