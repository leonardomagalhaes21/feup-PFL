{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
maxpos :: [Int] -> Int
maxpos xs = 
    let list = filter (>0) xs
    in if (length list > 0) 
        then maximum (list) 
        else 0


dups :: [a] -> [a] 
dups xs = daux xs True

daux :: [a] -> Bool -> [a]
daux [] _ = []
daux (x:xs) True = x : x : (daux xs False)
daux (x:xs) False = x : (daux xs True)


transforma :: String -> String
transforma "" = ""
transforma (x:xs) = if (x == 'a' ||x == 'e' ||x == 'i' ||x == 'o' ||x == 'u') 
                    then x : 'p' : x : transforma xs
                    else x : transforma xs


type Vector = [Int]
type Matriz = [[Int]]

transposta :: Matriz -> Matriz
transposta [] = []
transposta ([] : _) = []
transposta m = [map head m] ++ transposta (map tail m)

prodInterno :: Vector -> Vector -> Int
prodInterno [] [] = 0
prodInterno (x:xs) (y:ys) = x*y + prodInterno xs ys

prodMat :: Matriz -> Matriz -> Matriz
prodMat a b = prodMat' a  (transposta b)

prodMat' :: Matriz -> Matriz -> Matriz
prodMat' [] [] = []
prodMat' xs ys = [[(prodInterno a  b)| b <- ys ] | a <- xs]


data Arv a = F | N a (Arv a) (Arv a)
    deriving(Show)


alturas :: Arv a -> Arv Int
alturas F = F
alturas (N x esq dir) = N (altura (N x esq dir)) (alturas esq) (alturas dir)

altura :: Arv a -> Int
altura F = 0
altura (N x esq dir) = 1+ max  (altura esq)  (altura dir)


equilibrada :: Arv a -> Bool
equilibrada (N x esq dir) = altura (N x esq dir) - (altura esq) == 1 && altura (N x esq dir) - (altura dir) == 1


f :: (a -> b -> c) -> b -> a -> c
f g b a = g a b 