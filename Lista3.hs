-- ===========================================
agrupar :: (Eq a) => [a] -> [[a]]
agrupar [] = [[]]
agrupar (x:xs) = [[i | i <- (x:xs), i == x]] ++ agrupar (filter (/= x) xs)

-- ===========================================
type Lado = Double
type Triangulo = (Lado,Lado,Lado)

areaTriangulo :: Triangulo -> Double
areaTriangulo (a, b, c) = sqrt(e * (e - a) * (e - b) *( e - c)) where e = ((a + b + c)/2)

sumArea :: [Triangulo] -> Double
sumArea ls = foldr1 (+) (map areaTriangulo ls)
-- =========================================== 
type Nome = String

abrev :: Nome -> Nome
abrev [] = []
abrev (x:xs) = x : ". " ++ (reverse (takeWhile (/= ' ') (reverse (x:xs))))

abrevSort :: [Nome] -> [Nome]
abrevSort [] = []
abrevSort l@(x:xs) = mergesort (map abrev l)