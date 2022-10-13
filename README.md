# PFL_Project1

## To do:
- [x] polinomio -> string
- [x] derivada (implica divisao)
- [x] exemplos de utilizacao
- [x] ordenar variaveis -> o programa considera que "2xyz" é diferente de "2yzx"
- [x] Integrar ordenacao de variaveis com funcoes
- [x] Escrever relatorio
- [ ] Comentar codigo
- [x] resolver bug: `sumPolynoms [Termo 2 "y" [1], Termo 5.1 "y" [1], Termo 1 "y" [1]]`
- [ ] Aula: perguntar ao stor a questao de o polyToString imprimir plicas com as variaveis
- [x] Todos os resultados das funcoes deviam estar automaticamente normalizados
- [ ] Funcoes receberem string e chamarem elas o wordsplit? fazer
- [x] Bug: `wordSplit "2*x^3*y^2 - y + 3"`
- [x] Adicionar espacos no polyToString para ficar igual ao input do wordsplit
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
Recebe uma string correspondente a 1 termo e faz parse da mesma de forma a obter o valor do coeficiente, das variáveis usadas e dos expoentes de cada variável.

Exemplo: `termoFactory "-2*x^3*y^2"`

```haskell
polyToString :: Polynom -> String
```
Função responsável pela conversão da representação interna do polinómio para uma string percetível pelo utilizador.

Exemplo: `polyToString $ wordSplit "2*x^3*y^2 - 5*y + 3"`

#### Normalização de polinómios:
Função usada:
```haskell
normalize :: Polynom -> Polynom
```
Esta função recebe um polinómio e devolve o polinómio resultante de somar os seus termos (se possível), remover os termos com coeficiente nulo e de ordenar os termos de forma a ficarem por ordem decrescente de expoente e agrupados por variável. Para a ordenação, definimos um overload da classe Ord.

Exemplo: `normalize  [Termo 0 "x" [2], Termo 2 "y" [1], Termo 5.1 "z" [1], Termo 1 "y" [1], Termo 7 "y" [2]]`

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

Exemplo: `grouping [Termo 5 "y" [2], Termo 7 "y" [3], Termo 8 "z" [2], Termo 2 "y" [2]]`

```haskell
sumMatchingTerms :: [Termo] -> Termo
```
Recebe uma lista de termos que podem ser somados e soma-os recursivamente, devolvendo o termo resultante.

Exemplo: `sumMatchingTerms [Termo 2 "y" [1], Termo 1 "y" [1], Termo 5 "y" [1]]`

```haskell
sumPolynoms :: Polynom -> Polynom
```
Esta é a função principal desta funcionalidade. Recebe uma lista de termos (polinómio) e soma todos os termos que forem compatíveis, recorrendo às funções descritas acima. 

Exemplo: `sumPolynoms [Termo 0 "x" [2], Termo 2 "y" [1], Termo 5.1 "z" [1], Termo 1 "y" [1], Termo 7 "y" [2], Termo 2 "y" [1]]`

#### Multiplicação de polinómios:
Funções usadas:

```haskell
multiplyPolynoms :: Polynom -> Polynom -> Polynom
```
Esta é a função principal desta funcionalidade. Recebe dois polinómios e devolve o resultado da multiplicação dos dois, recorrendo a uma lista em compreensão e à função multiplyTerms que está explicada abaixo.

Exemplo (propriedade distributiva): `multiplyPolynoms [Termo 1 "a" [1], Termo 1 "b" [1]] [Termo 1 "c" [1], Termo 1 "d" [1]]`

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