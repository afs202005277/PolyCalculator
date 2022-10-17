# PFL_Project1

## To do:
- [x] polinomio -> string
- [x] derivada (implica divisao)
- [x] exemplos de utilizacao
- [x] ordenar variaveis -> o programa considera que "2xyz" é diferente de "2yzx"
- [x] Integrar ordenacao de variaveis com funcoes
- [x] Escrever relatorio - FALTA O findExponents
- [x] Comentar codigo - FALTA O findExponents
- [x] resolver bug: `sumPolynoms [Termo 2 "y" [1], Termo 5.1 "y" [1], Termo 1 "y" [1]]`
- [x] Aula: perguntar ao stor a questao de o polyToString imprimir plicas com as variaveis
- [x] Todos os resultados das funcoes deviam estar automaticamente normalizados
- [x] Funcoes receberem string e chamarem elas o wordsplit? fazer
- [x] Bug: `wordSplit "2*x^3*y^2 - y + 3"`
- [x] Adicionar espacos no polyToString para ficar igual ao input do wordsplit
- [x] Completar relatorio com a derivada
- [ ] TESTAR TUDO E MAIS ALGUMA COISA

### Representação interna dos polinómios:

Na resolução deste projeto, usamos a seguinte representação interna de polinómios:
```haskell
type Polynom = [Termo]
data Termo = Termo {coef :: Float, variable :: String, expo :: [Int]}
```
Ou seja, 1 polinómio é representado por uma lista de termos, sendo que, cada termo é composto por um coeficiente de vírgula flutuante, uma lista de variáveis e uma lista de expoentes. A associação entre as variáveis e os expoentes é feita recorrendo aos indíces de cada um, ou seja, o expoente que se encontra no indíce 2 é o expoente da variável no indíce 2 da respetiva lista. Por exemplo:
> 2\*y\*z + 5\*x\*z + y + 7\*a\^1\*b\^2\*c^3 é representado por:
> [Termo {c = 2.0, v = "yz", e = [1,1]},Termo {c = 5.0, v = "xz", e = [1,1]},Termo {c = 1.0, v = "y", e = [1]},Termo {c = 7.0, v = "abc", e = [1,2,3]}]

Escolhemos esta representação devido ao facto de ser aquela que melhor separa as diferentes componentes de um polinómio e por ser aquela que vai mais ao encontro da definição matemática do mesmo: um conjunto de **termos** interligados por operações de adição e subtração, sendo que cada termo é composto por um **coeficiente**, um conjunto (possivelmente vazio) de **variáveis** e os respetivos **expoentes**.
Pelos motivos acima apresentados, decidimos que esta seria uma boa maneira de representar internamente os polinómios.

### Descrição de implementação de funcionalidades
```haskell
wordSplit :: String -> Polynom
```
Função responsável por converter o input do utilizador (polinómio representado por uma string) na representação interna adotada neste projeto, "partindo" a string nos sinais "+" e "-" e enviando cada partição à função termoFactory que, por sua vez, devolve o termo correspondente a essa partição.

Exemplo: `wordSplit "2*x^3*y^2 - 5*y + 3"`

```haskell
termoFactory :: String -> Termo
```
Função auxiliar da função wordSplit que recebe uma string correspondente a 1 termo e faz parse da mesma de forma a obter o valor do coeficiente, das variáveis usadas e dos expoentes de cada variável.

Exemplo: `termoFactory "-2*x^3*y^2"`

```haskell
findExponents :: String -> [Int]
```
Função auxiliar usada pela função termoFactory que recebe uma string que contém apenas as variáveis e os expoentes das mesmas e devolve os expoentes de cada variável.

Exemplo: `findExponents "x2y3"`

```haskell
variableWithExpo :: String -> [Int] -> String
```
Função auxiliar do polyToString que cria a string que adiciona a variavel ao expoente.
Por exemplo: variableWithExpo "xy" [2, 1] retorna "x^2*y"

Exemplo: `variableWithExpo "xy" [2, 1]`

```haskell
termoToString :: Termo -> String
```
Função auxiliar do polyToString que adiciona o coeficiente ao resultado do variableWithExpo. Com a exceção de quando o coeficiente é 1.
Por exemplo: termoToString (Termo 3 "xy" [2, 1]) retorna "3.0*x^2*y"

Exemplo: `termoToString (Termo 3 "xy" [2, 1])`

```haskell
polyToString :: Polynom -> String
```
Função responsável pela conversão da representação interna do polinómio para uma string percetível pelo utilizador.

Exemplo: `polyToString $ wordSplit "2*x^3*y^2 - 5*y + 3"`

```haskell
sortGT :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Ordering
```
Função auxiliar do organize que ordena por ordem crescente as variáveis com os seus respetivos expoentes.

Exemplo: `sortGT ("x", 2) ("y", 3)`

```haskell
organize :: Termo -> Termo
```
Função responsável por organizar as variáveis dentro dos termos por ordem alfabetica. Necessário para o programa não achar que "xy" é diferente de "yx"

Exemplo: `organize $ Termo 3 "zyx" [3, 2, 1]`

#### Normalização de polinómios:
Função usada:
```haskell
normalize :: String -> Polynom
```
Esta função recebe um string de um polinómio e devolve o polinómio resultante de somar os seus termos (se possível), remover os termos com coeficiente nulo e de ordenar os termos de forma a ficarem por ordem decrescente de expoente e agrupados por variável. Para a ordenação, definimos um overload da classe Ord.

Exemplo: `normalize "3*y^2 + 0*x + 5*y + 3*y + 2*y^2"`

#### Soma de polinómios:
Funções usadas:
```haskell
isSummable :: Termo -> Termo -> Bool
```
Retorna um booleano que indica se os dois termos podem ser somados ou não, por outras palavras, verifica se os dois termos têm as mesmas variáveis e expoentes.

Exemplos:
- `isSummable (Termo 5 "x" [3]) (Termo 10 "y" [3])`
- `isSummable (Termo 5 "y" [5]) (Termo 10 "y" [3])`
- `isSummable (Termo 5 "y" [3]) (Termo 10 "y" [3])`

```haskell
grouping :: Polynom -> [Polynom]
```
Retorna uma lista de polinómios, ou seja, uma lista de listas de termos sendo que os elementos de cada sublista são elementos que podem ser somados entre si (esta função recorre à função isSummable para agrupar os elementos que podem ser somados).

Exemplo: `grouping $ Polynom ([Termo 5 "y" [2], Termo 7 "y" [3], Termo 8 "z" [2], Termo 2 "y" [2]])`

```haskell
sumMatchingTerms :: [Termo] -> Termo
```
Recebe uma lista de termos que podem ser somados e soma-os recursivamente, devolvendo o termo resultante.

Exemplo: `sumMatchingTerms [Termo 2 "y" [1], Termo 1 "y" [1], Termo 5 "y" [1]]`

```haskell
sumPolynoms :: [String] -> Polynom
```
Esta é a função principal desta funcionalidade. Recebe uma polinómio no formato de string e soma todos os termos que forem compatíveis, recorrendo às funções descritas acima. 

Exemplo: `sumPolynoms ["2*x + 5*y", "8*x + 2*y"]`

#### Multiplicação de polinómios:
Funções usadas:

```haskell
multiplyPolynoms :: String -> String -> Polynom
```
Esta é a função principal desta funcionalidade. Recebe dois polinómios no formato de string e devolve o resultado da multiplicação dos dois, recorrendo a uma lista em compreensão e à função multiplyTerms que está explicada abaixo.

Exemplo (propriedade distributiva): `multiplyPolynoms "2*x + 5*y" "6*y + 9*x"`

```haskell
multiplyTerms :: Termo -> Termo -> Termo
```
Esta função recebe dois termos e retorna o termo resultante da multiplicação dos dois: os coeficientes são multiplicados entre si, a lista de variáveis é atualizada com a concatenação das variáveis dos dois termos envolvidos (variáveis repetidas são eliminadas, ficando apenas presente 1 exemplar dessa variável, recorrendo à função nub do Prelúdio padrão) e somam-se os expoentes. 
Para se obter a lista de variáveis e expoentes resultantes, recorremos às funções collapseExponents e expandExponents explicadas abaixo.

Exemplo: `multiplyTerms (Termo 5 "xy" [2, 1]) (Termo 10 "yz" [1, 3])`

```haskell
expandExponents :: String -> [Int] -> String
```
Recebe a lista de variáveis e de expoentes de um termo e retorna uma string em que cada variável está repetida n vezes, em que n = expoente da variável. 
Por exemplo: expandExponents "xyz" [1, 2, 3] retorna "xyyzzz".

Exemplo: `expandExponents "xyz" [1, 2, 3]`

```haskell
collapseExponents :: String -> [Int]
```
Esta função faz parcialmente o inverso da expandExponents, ou seja, recebe uma string e devolve a lista de expoentes. 
Por exemplo: collapseExponents "xyyzzz" retorna [1, 2, 3]

Exemplo: `collapseExponents "xyyzzz"`

Ideia geral da multiplicação: multiplicam-se os coeficientes, expande-se os expoentes de cada termo (função expandExponents) e concatena-se a expansão resultante. No final, remove-se os duplicados (obtendo assim a lista de variáveis final) e recalcula-se os expoentes (função collapseExponents).

#### Derivada de um polinómio:
Funções usadas:

Ideia geral da derivação: procura-se os expoentes da variável que se pretende derivar (findVar), multiplica-se o coeficiente por esse valor e decrementa-se o valor dos expoentes dessa variável. No caso do expoente ficar a zero, a variável é retirada (removeZeroExp).

```haskell
derivative :: String -> Char -> Polynom
```

Esta é a função principal. Recebe um polinómio no formato de string e uma variável a derivar e usa a função findVar (para encontrar o atual expoente da variável e arranjar os expoentes da derivada) e removeZeroExp (para remover as variáveis cujo expoente devido a derivada pode ter ido para zero) para efetuar a derivada da variável dada.
Por exemplo: derivative "2*x^2*y + 5*x" 'x' retorna [Termo {coef = 5.0, variable = "", expo = []},Termo {coef = 4.0, variable = "xy", expo = [1,1]}]

Exemplo: `derivative "2*x^2*y + 5*x" 'x'`

```haskell
removeZeroExp :: String -> [Int] -> (String, [Int])
```

Esta função trata de remover as variáveis que têm expoente zero, Ou seja, se tivermos um termo com x^0 = 1, esta variável é removida.
Por exemplo: removeZeroExp "xyz" [3,0,2] retorna ("xz", [3,2])

Exemplo: `removeZeroExp "xyz" [3,0,2]`

```haskell
findVar :: String -> [Int] -> Char -> (Int, [Int])
```

Esta função trata de descobrir o expoente da variável dada e devolve a lista de expoentes atualizada após uma derivação em ordem à variável dada.
Por exemplo: findVar "xyz" [3, 2, 3] 'x' retorna (3, [2, 2, 3])

Exemplo: `findVar "xyz" [3, 2, 3] 'x'`
