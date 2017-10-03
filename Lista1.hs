mdc :: Int -> Int -> Int
mdc x y | (mod x y) == 0 = y
        | otherwise = mdc y (mod x y)
-- ==================================
isPrime :: Int -> Bool
isPrime x | x == 2 = True
          | otherwise = null [y | y <- [2..floor (sqrt (fromIntegral x))], mod x y == 0]
-- ==================================
type Ponto = (Double, Double, Double)

distancia :: Ponto -> Ponto -> Double
distancia (a,b,c) (d,e,f) = sqrt(((a - d) ^ 2) + ((b - e) ^ 2) + ((c - f) ^ 2))
-- =================================
sum100square = sum[x ^ 2 | x <- [1..100]]
-- ================================
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n , x /= y]
-- ================================
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
 |(x<=y) = x : (merge xs (y:ys))
 |otherwise = y: (merge ys (x:xs))

mergeSort :: Ord t => [t] -> [t]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (take(div (length xs)  2) xs)) (mergeSort (drop(div (length xs) 2 ) xs))

-- ====================================

data DiaSemana = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado
  deriving (Eq, Ord, Show, Enum)

util :: DiaSemana -> Bool
util dia = dia > Domingo && dia < Sabado

ordenaUteis :: [DiaSemana] -> [DiaSemana]
ordenaUteis l = mergeSort (filter util l)

datasIguais :: [(DiaSemana,Int)] -> DiaSemana -> [Int]
datasIguais [] _ = []
datasIguais (x:xs) dia | a == dia = b : datasIguais xs dia
                       | otherwise = datasIguais xs dia
  where (a, b) = x

imprimeMes :: DiaSemana -> [(Int, DiaSemana)]
imprimeMes dia = zip [1..30] ([dia .. Sabado] ++ (cycle [Domingo .. Sabado]))
