import Data.Char
import Data.List
import Data.Maybe (fromJust, isJust)

data Termo = Termo Coef Variable Exp

type Coef = Float
type Variable = String
type Exp = Float

convertToExp :: String -> Int
convertToExp str
            | all isDigit str = read str :: Int
            | otherwise = 1

wordSplit :: String -> [[String]]
wordSplit str = [[takeWhile isDigit uterm, takeWhile isAlpha (dropWhile (\chr -> isDigit chr || chr == '*') uterm), convertToExp (dropWhile (const False) (drop idx uterm))] | uterm <- words str, let idx = if Data.Maybe.isJust (elemIndex '^' uterm) then fromJust(elemIndex '^' uterm)+1 else 0, length uterm > 1]

{-
stringToInternal :: [Termo] -> IO ()
stringToInternal = do putStr "Polinómio?"
                      str <- getLine
                      -}