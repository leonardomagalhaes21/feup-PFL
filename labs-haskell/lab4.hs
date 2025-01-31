import Language.Haskell.TH (safe, cApi, Exp)
import Distribution.Simple.Program.HcPkg (list)

data Arv a = Vazia | No a (Arv a) (Arv a) deriving(Show)

--4.1)
sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No x esquerda direita) = x + sumArv esquerda + sumArv direita

--4.2)
listar :: Arv a -> [a]
listar Vazia = []
listar (No v e d) = listar e ++ [v] ++ listar d

listarDesc :: Arv a -> [a]
listarDesc Vazia = []
listarDesc (No v e d) = listarDesc d ++ [v] ++ listarDesc e

--4.3)
nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No x _ _) = [x]
nivel n (No _ e d) = nivel(n-1) e ++ nivel(n-1) d

--4.5)
mapArv :: (a -> b) -> Arv a -> Arv b
mapArv _ Vazia = Vazia
mapArv f (No val esq dir) = No (f val) (mapArv f esq) (mapArv f dir)

--4.6)
mais_esq :: Arv a -> a
mais_esq (No x Vazia _) = x
mais_esq (No _ esq _) = mais_esq esq

mais_dir :: Arv a -> a
mais_dir (No x _ Vazia) = x
mais_dir (No _ _ dir) = mais_dir dir

remover :: Ord a => a -> Arv a -> Arv a
remover _ Vazia = Vazia 
remover x (No y esq Vazia)
    | x==y = esq
remover x (No y Vazia dir)
    | x==y = dir
remover x (No y esq dir)
    | x < y = No y (remover x esq) dir
    | x > y = No y esq (remover x dir)
    | x == y = let z = mais_dir esq
            in No z (remover z esq) dir


data Expr = Lit Integer | Op Ops Expr Expr
data Ops = Add | Sub | Mul | Div

eval :: Expr -> Integer
eval (Lit n) = n
eval (Op op esq dir) = case op of
    Add -> eval esq + eval dir
    Sub -> eval esq - eval dir
    Mul -> eval esq * eval dir
    Div -> eval esq `div` eval dir
    