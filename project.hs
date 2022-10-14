import Data.Char
import Data.Function (on)
import Data.List
import Data.Maybe
import Text.Read (readMaybe)



-- 1 polinómio é representado por uma lista de termos, sendo que, cada termo é composto por um coeficiente de vírgula flutuante, uma lista de variáveis e uma lista de expoentes.
-- A associação entre as variáveis e os expoentes é feita recorrendo aos indíces de cada um, ou seja, o expoente que se encontra no indíce 2 é o expoente da variável no indíce 2 da respetiva lista.
data Polynom = Polynom [Termo]
data Termo = Termo {coef :: Float, variable :: String, expo :: [Int]} deriving (Eq)

instance Show Polynom where
  show poly = polyToString poly

instance Ord Termo where
  compare t1 t2
    | variable t1 < variable t2 = LT
    | variable t1 == variable t2 && expo t1 < expo t2 = GT
    | variable t1 == variable t2 && expo t1 == expo t2 = EQ
    | otherwise = GT

extractListOfTerms :: Polynom -> [Termo]
extractListOfTerms (Polynom lst) = lst

{-
Esta função recebe um string de um polinómio e devolve o polinómio resultante de somar os seus termos (se possível), 
remover os termos com coeficiente nulo e de ordenar os termos de forma a ficarem por ordem decrescente de expoente e agrupados por variável. 
Para a ordenação, definimos um overload da classe Ord.
-}
normalize :: String -> Polynom
normalize p = Polynom (sort (filter (\x -> coef x /= 0) $ extractListOfTerms (sumPolynoms [p])))

{-
Retorna um booleano que indica se os dois termos podem ser somados ou não, por outras palavras, verifica se os dois termos têm as mesmas variáveis e expoentes.
-}
isSummable :: Termo -> Termo -> Bool
isSummable t1 t2 = variable t1 == variable t2 && expo t1 == expo t2

{-
Retorna uma lista de polinómios, ou seja, uma lista de listas de termos sendo que os elementos de cada sublista são elementos que podem ser somados entre si
(esta função recorre à função isSummable para agrupar os elementos que podem ser somados).
-}
grouping :: Polynom -> [[Termo]]
grouping (Polynom []) = []
grouping (Polynom (x : xs)) = filter (isSummable x) (x : xs) : grouping (Polynom $ filter (not . isSummable x) xs)


findExponents :: String -> [Int]
findExponents [] = []
findExponents [a] = [1]
findExponents (x : y : xs)
  | isAlpha x && isDigit y = (read (takeWhile isDigit (y : xs)) :: Int) : findExponents xs
  | otherwise = 1 : findExponents (y : xs)


{-
Recebe uma string correspondente a 1 termo e faz parse da mesma de forma a obter o valor do coeficiente, das variáveis usadas e dos expoentes de cada variável.
-}
termoFactory :: String -> Termo
termoFactory str = organize (Termo (fromMaybe 1 attempt_coef) (filter isAlpha str) (findExponents (filter (\x -> isAlpha x || isDigit x) (dropWhile (\x -> (not . isAlpha) x && x /= '*') str))))
  where
    attempt_coef = readMaybe (if length coef_component == 1 && head coef_component == '-' then "-1" else coef_component) :: Maybe Float
    coef_component = takeWhile (\x -> (not . isAlpha) x && x /= '*') str


{- 
Função responsável por converter o input do utilizador (polinómio representado por uma string) na representação interna adotada neste projeto,
"partindo" a string nos sinais "+" e "-" e enviando cada partição à função termoFactory que, por sua vez, devolve o termo correspondente a essa partição. 
-}
wordSplit :: String -> Polynom
wordSplit str =
  Polynom [ termoFactory (if signal == '+' then uterm else signal : uterm) | idx_term <- [0 .. length (words str) -1], let uterm = words str !! idx_term
                                                                                                                           signal = if idx_term > 0 && head (words str !! (idx_term -1)) == '-' then '-' else '+', uterm /= "+" && uterm /= "-"
  ]

{-
Recebe uma lista de termos que podem ser somados e soma-os recursivamente, devolvendo o termo resultante.
-}
sumMatchingTerms :: [Termo] -> Termo
sumMatchingTerms [] = Termo 0 "" []
sumMatchingTerms [p] = p
sumMatchingTerms (p1 : p2 : ps) = sumMatchingTerms (recent_term : ps)
  where
    recent_term = Termo (coef p1 + coef p2) (variable p1) (expo p1)

{-
Esta é a função principal desta funcionalidade. Recebe uma polinómio no formato de string e soma todos os termos que forem compatíveis, recorrendo às funções descritas acima.
-}
sumPolynoms :: [String] -> Polynom
sumPolynoms p = Polynom $ map sumMatchingTerms (grouping (Polynom (concatMap extractListOfTerms (map wordSplit p))))

{-
Recebe a lista de variáveis e de expoentes de um termo e retorna uma string em que cada variável está repetida n vezes,
em que n = expoente da variável. Por exemplo: expandExponents "xyz" [1, 2, 3] retorna "xyyzzz".
-}
expandExponents :: String -> [Int] -> String
expandExponents [] _ = []
expandExponents _ [] = []
expandExponents (x : xs) (n : ns) = replicate n x ++ expandExponents xs ns

{-
Esta função faz parcialmente o inverso da expandExponents, ou seja, recebe uma string e devolve a lista de expoentes.
Por exemplo: collapseExponents "xyyzzz" retorna [1, 2, 3]
-}
collapseExponents :: String -> [Int]
collapseExponents [] = []
collapseExponents (x : xs) = length (filter (x ==) (x : xs)) : collapseExponents (filter (x /=) xs)


{-
Esta função recebe dois termos e retorna o termo resultante da multiplicação dos dois: os coeficientes são multiplicados entre si,
a lista de variáveis é atualizada com a concatenação das variáveis dos dois termos envolvidos (variáveis repetidas são eliminadas,
ficando apenas presente 1 exemplar dessa variável, recorrendo à função nub do Prelúdio padrão) e somam-se os expoentes.
Para se obter a lista de variáveis e expoentes resultantes, recorremos às funções collapseExponents e expandExponents explicadas abaixo.
-}
multiplyTerms :: Termo -> Termo -> Termo
multiplyTerms t1 t2 = Termo (coef t1 * coef t2) (nub variables) (collapseExponents variables)
  where
    variables = expandExponents (variable t1) (expo t1) ++ expandExponents (variable t2) (expo t2)


{-
Esta é a função principal desta funcionalidade. Recebe dois polinómios no formato de string e devolve o resultado da multiplicação dos dois, 
recorrendo a uma lista em compreensão e à função multiplyTerms que está explicada abaixo.
-}
multiplyPolynoms :: String -> String -> Polynom
multiplyPolynoms pol1 pol2 = Polynom [multiplyTerms t1 t2 | t1 <- p1, t2 <- p2]
  where
    p1 = extractListOfTerms $ wordSplit pol1
    p2 = extractListOfTerms $ wordSplit pol2

{-
Função auxiliar do polyToString que cria a string que adiciona a variavel ao expoente.
Por exemplo: variableWithExpo "xy" [2, 1] retorna "x^2*y"
-}
variableWithExpo :: String -> [Int] -> String
variableWithExpo [] [] = ""
variableWithExpo var exp
  | head exp == 1 = "*" <> [head var] <> variableWithExpo (tail var) (tail exp)
  | otherwise = "*" <> [head var] <> "^" <> show (head exp) <> variableWithExpo (tail var) (tail exp)

{-
Função auxiliar do polyToString que adiciona o coeficiente ao resultado do variableWithExpo. Com a exceção de quando o coeficiente é 1.
Por exemplo: termoToString (Termo 3 "xy" [2, 1]) retorna "3.0*x^2*y"
-}
termoToString :: Termo -> String
termoToString ter
            | coef ter == 1 = tail $ variableWithExpo (variable ter) (expo ter)
            | otherwise = show (coef ter) <> variableWithExpo (variable ter) (expo ter)

{-
Função responsável pela conversão da representação interna do polinómio para uma string percetível pelo utilizador.
-}
polyToString :: Polynom -> String
polyToString p
  | length pol == 1 = termoToString (last pol)
  | head (termoToString (last pol)) == '-' = polyToString (Polynom $ init pol) <> " - " <> tail (termoToString (last pol))
  | otherwise = polyToString (Polynom $ init pol) <> " + " <> termoToString (last pol)
  where pol = extractListOfTerms $ normalize $ polyToString (Polynom $ map organize (extractListOfTerms p))


{-
Função auxiliar do organize que ordena por ordem crescente as variáveis com os seus respetivos expoentes.
-}
sortGT :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Ordering
sortGT (a1, b1) (a2, b2)
  | a1 < a2 = LT
  | a1 > a2 = GT
  | a1 == a2 = compare b1 b2

{-
Função responsável por organizar as variáveis dentro dos termos por ordem alfabetica. Necessário para o programa não achar que "xy" é diferente de "yx"
-}
organize :: Termo -> Termo
organize ter = Termo (coef ter) org_var org_exp where (org_var, org_exp) = unzip (sortBy sortGT (zip (variable ter) (expo ter)))

{-
Esta função trata de remover as variáveis que têm expoente zero, Ou seja, se tivermos um termo com x^0 = 1, esta variável é removida.
Por exemplo: removeZeroExp "xyz" [3,0,2] retorna ("xz", [3,2])
-}
removeZeroExp :: String -> [Int] -> (String, [Int])
removeZeroExp "" [] = ("", [])
removeZeroExp var expo
  | head expo == 0 = removeZeroExp (tail var) (tail expo)
  | otherwise = (head var : altvar, head expo : altexpo)
  where
    (altvar, altexpo) = removeZeroExp (tail var) (tail expo)

{-
Esta função trata de descobrir o expoente da variável dada e devolve a lista de expoentes atualizada após uma derivação em ordem à variável dada.
Por exemplo: findVar "xyz" [3, 2, 3] 'x' retorna (3, [2, 2, 3])
-}
findVar :: String -> [Int] -> Char -> (Int, [Int])
findVar "" expo _ = (-1, expo)
findVar var expo lookingfor
  | head var == lookingfor = (head expo, head expo -1 : tail expo)
  | otherwise = (i, head expo : e)
  where
    (i, e) = findVar (tail var) (tail expo) lookingfor


{-
Esta é a função principal. Recebe um polinómio no formato de string e uma variável a derivar e usa a função findVar (para encontrar o atual expoente da variável e arranjar os expoentes da derivada)
e removeZeroExp (para remover as variáveis cujo expoente devido a derivada pode ter ido para zero) para efetuar a derivada da variável dada.
Por exemplo: derivative "2x^2y + 5*x" 'x' retorna [Termo {coef = 5.0, variable = "", expo = []},Termo {coef = 4.0, variable = "xy", expo = [1,1]}]
-}
derivative :: String -> Char -> Polynom
derivative p var = normalize $ polyToString $ Polynom
  [ Termo (coef ter * fromIntegral coefmult) fixedvar fixedexp | ter <- pol, let (coefmult, defexpos) = findVar (variable ter) (expo ter) var
                                                                                 (fixedvar, fixedexp) = removeZeroExp (variable ter) defexpos, coefmult /= -1
  ] where pol = extractListOfTerms $ wordSplit p
