ehMatriz :: [[Int]] -> Bool
ehMatriz m = valorIgual (map length m)

valorIgual :: [Int] -> Bool
valorIgual (x:[]) = True
valorIgual (x:y:[]) = x == y
valorIgual(x:y:ys) = x == y && valorIgual ys

-- b)

diagonal :: [[Int]] -> [Int]
diagonal [] = []
diagonal l@(x:xs) = head x : diagonal (map (drop 1) xs)

-- c)
troca :: [[Int]] -> Int -> Int -> [[Int]]
troca l@(a:as) x y = (take (x-1) l) ++ ([l!!(y-1)]) ++ (drop x (take (y-1) l)) ++ ([l!!(x-1)]) ++ (drop y l)

--Quest2

type Codigo = Int
data Voto = Presidente Codigo | Conselheiro Codigo | Secretario Codigo | Branco deriving (Show)

instance Eq Voto where
 (==) (Presidente p1) (Presidente p2) = p1 == p2
 (==) (Conselheiro c1) (Conselheiro c2) = c1 == c2
 (==) (Secretario s1) (Secretario s2) = s1 == s2
 (==) (Branco) (Branco) = True
 (==) _ _ = False

type Urna = [Voto]
type Apuracao = [(Voto, Int)]

-- b)
totalVotos :: Urna -> Voto -> Int
totalVotos u v = length (filter (==v) u)

-- c)
apurar :: Urna -> Apuracao
apurar [] = []
apurar l@(x:xs) = (x, (totalVotos l x)) : apurar (filter (/=x) xs)

-- Quest3

data Pilha t = Pilha [t] | Vazia deriving (Show)

--b)
push :: Pilha t -> t -> Pilha t
push Vazia value = Pilha [value]
push (Pilha p) value = (Pilha (value:p))

pop :: Pilha t -> Pilha t
pop Vazia = Vazia
pop (Pilha (x:xs)) = (Pilha xs)

-- Quest 4

data Arv t = VaziaT | Arv t (Arv t) (Arv t) deriving (Show, Eq, Read)

insert :: (Ord t) => Arv t -> t -> Arv t
insert VaziaT val = Arv val VaziaT VaziaT
insert (Arv n l r) val
 | n == val = Arv val l r
 | val < n = Arv n (insert l val) r			
 | otherwise = Arv n l (insert r val)
 
arvLista :: Arv t -> [t]
arvLista VaziaT = []
arvLista (Arv n l r) = arvLista l ++ [n] ++ arvLista r

sumLista :: Num t => Arv t -> Int
sumLista arv = sum (arvLista arv)

sumLista1 :: Num t => Arv t -> Int
sumLista1 VaziaT = 0
sumLista1 (Arv n l r) = n + (sumLista1 l) + (sumLista1 r)
 
listArv :: Ord t => [t] -> Arv t
listArv [] = VaziaT
listArv l@(x:xs) = Arv x (listArv (filter (<x) xs)) (listArv (filter (>x) xs))
