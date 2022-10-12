import Data.Char
import Data.List
import Data.Maybe
import Text.Read (readMaybe)

type Polynom = [Termo]

data Termo = Termo {coef :: Float, variable :: String, expo :: [Int]} deriving (Eq, Show)




{-matchExponents :: Termo -> Termo -> Termo
matchExponents t1 t2 = Termo (coef t1 * coef t2)-}