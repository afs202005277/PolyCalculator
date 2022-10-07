import Data.Char
import Data.List
import Data.Maybe (fromJust, isJust)

type Polynom = [Termo]

data Termo = Termo { coef :: Int, variable :: String, expo :: Int } deriving (Eq, Show)

sumToIndex :: Int -> Int -> Int -> Polynom -> Polynom
sumToIndex _ _ _ [] = error "EMPTY POLYNOM"
sumToIndex element idx currentIdx (p:ps)
                          | idx == currentIdx = Termo (coef p + element) (variable p) (expo p) : ps
                          | otherwise = p : sumToIndex element idx (currentIdx+1) ps

findMatchingTerm :: Termo -> Polynom -> Int
findMatchingTerm _ [] = -1
findMatchingTerm needle (p:ps)
                    | coef p == coef needle && variable p == variable needle = 0
                    | otherwise = 1 + findMatchingTerm needle (p:ps)


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
addTerms [] = []
addTerms (p:ps)
            | Data.Maybe.isJust possibleIdx= addTerms (sumToIndex(coef p) (fromJust possibleIdx) 0 ps)
            | otherwise = p : addTerms ps
            where lista = [variable tmp | tmp <- p:ps]
                  possibleIdx = elemIndex (head lista) (tail lista)



{-
stringToInternal :: [Termo] -> IO ()
stringToInternal = do putStr "Polinómio?"
                      str <- getLine
                      -}