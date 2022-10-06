import Data.Char
import Data.List
import Data.Maybe (fromJust, isJust)

data Termo = Termo Sign Coef Variable Exp deriving (Eq, Show)

type Sign = Char

type Coef = Int

type Variable = String

type Exp = Int

convertToExp :: String -> Int
convertToExp [] = 1
convertToExp (x:xs)
  | all isDigit (x:xs) = read (x:xs) :: Int
  | otherwise = 1

wordSplit :: String -> [Termo]
wordSplit str = [Termo signal (convertToExp ((takeWhile isDigit uterm))) (takeWhile isAlpha (dropWhile (\chr -> isDigit chr || chr == '*') uterm)) (convertToExp (dropWhile (const False) (drop idx uterm))) | idx_term <- [0..length (words str)-1], let uterm = words str !! idx_term; idx = if Data.Maybe.isJust (elemIndex '^' uterm) then fromJust (elemIndex '^' uterm) + 1 else 0; signal = if idx_term > 0 && head (words str !! (idx_term-1)) == '-' then '-' else '+', uterm /= "+" && uterm /= "-"]

{-
stringToInternal :: [Termo] -> IO ()
stringToInternal = do putStr "Polinómio?"
                      str <- getLine
                      -}