grouping :: [Int] -> [[Int]]
grouping [] = []
grouping (x:xs) = filter (x==) (x:xs) : grouping (filter (x/=) xs) 