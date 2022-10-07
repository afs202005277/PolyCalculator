import Data.Char
import Data.List
import Data.Maybe (fromJust, isJust)

type Polynom = [Termo]

data Termo = Termo { coef :: Int, variable :: String, exp :: Int } deriving (Eq, Show)

(!!) :: (Int, String, Int) -> Int -> Either Int String
(!!) (a:b:c:_) idx
            | idx == 0 = a
            | idx == 1 = b
            | idx == 2 = c
            | otherwise = error "OUT OF BOUNDS"

sumToIndex :: Int -> Int -> Int -> Polynom -> Polynom
sumToIndex element idx currentIdx (p:ps)
                          | idx == currentIdx = Termo ((extractValues p Main.!! 0) + element) ((extractValues p) Main.!! 1) ((extractValues p) Main.!! 2) : ps
                          | otherwise = p : sumToIndex element idx (currentIdx+1) ps


extractValues :: Termo -> (Int, String, Int)
extractValues (Termo a b c) = (a, b, c)

getVariableList :: Polynom -> [String]
getVariableList p = [extractValues tmp Main.!! 1 | tmp <- p]

convertToExp :: String -> Int
convertToExp [] = 1
convertToExp (x : xs)
  | x == '+' && not (null xs) = read xs :: Int
  | x == '-' && not (null xs) = read (x : xs) :: Int
  | x == '-' && null xs = -1
  | all isDigit (x : xs) = read (x : xs) :: Int
  | otherwise = 1

wordSplit :: String -> Polynom
wordSplit str = [Termo (convertToExp (signal : takeWhile isDigit uterm)) (takeWhile isAlpha (dropWhile (\chr -> isDigit chr || chr == '*') uterm)) (convertToExp (dropWhile (const False) (drop idx uterm))) | idx_term <- [0 .. length (words str) -1], let uterm = words str Prelude.!! idx_term; idx = if Data.Maybe.isJust (elemIndex '^' uterm) then fromJust (elemIndex '^' uterm) + 1 else 0; signal = if idx_term > 0 && head (words str Prelude.!! (idx_term -1)) == '-' then '-' else '+', uterm /= "+" && uterm /= "-"]

addTerms :: Polynom -> Polynom
addTerms (p1:ps)
            | Data.Maybe.isJust(elemIndex (head lista) (tail lista)) = addTerms sumToIndex(extractValues p1 Main.!! 0, fromJust(elemIndex (head lista) (tail lista)), 0, ps)



{-
stringToInternal :: [Termo] -> IO ()
stringToInternal = do putStr "Polinómio?"
                      str <- getLine
                      -}