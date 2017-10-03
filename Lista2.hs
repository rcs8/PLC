fat :: Int -> [Int]
fat n = [product [1..x] | x <- [1..n] ]
-- ======================================
testaLista ::( a −> Bool) −> [a] −> Bool
testaLista f [] = True
testaLista f (x:xs) = (f x) && (testaLista f xs)

testaListaM :: (a -> Bool) -> [a] -> Bool
testaListaM f l = and(map f l)

testaListaF :: (a -> Bool) -> [a] -> Bool
testaListaF f l = foldr (\a b -> f a && b) True l
