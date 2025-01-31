maxpos :: [Int] -> Int
maxpos [] = 0
maxpos xs = if maximum xs <= 0 then 0 else maximum xs

dups :: [a] -> [a]
dups [] = []
dups (x:xs) = x : x : dupsaux xs False

dupsaux :: [a] -> Bool -> [a]
dupsaux [] _ = []
dupsaux (x:xs) False = x : dupsaux xs True
dupsaux (x:xs) True = x: x : dupsaux xs False

transforma :: String -> String
transforma [] = []
transforma (x:xs) = if x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u' 
                    then x : 'p' : x : transforma xs 
                    else x: transforma xs

type Vector = [Int]
type Matriz = [[Int]]

transposta :: Matriz -> Matriz
transposta [] = []
transposta [[],_] = []
transposta m = (map head m) : transposta (map tail m)

prodInterno :: Vector -> Vector -> Int
prodInterno [] [] = 0
prodInterno (x:xs) (y:ys) = x*y + (prodInterno xs ys)

prodMat :: Matriz -> Matriz -> Matriz
prodMat m1 m2 = [[(prodInterno a b)| b <- (transposta m2)] | a <- m1] 

data Arv a = F | N a (Arv a) (Arv a)
    deriving(Show)

alturas :: Arv a -> Arv Int
alturas F = F
alturas (N k esq dir) = N i (alturas esq) (alturas dir)
    where i = altura (N k esq dir)

altura :: Arv a -> Int
altura F = 0
altura (N _ esq dir) = 1 + max (altura esq) (altura dir)


equilibrada :: Arv a -> Bool
equilibrada F = True
equilibrada (N k esq dir) = if ( abs (altura esq - altura dir) > 1)
                            then False
                            else (equilibrada esq) && (equilibrada dir)

f :: (a -> b -> c) -> b -> a -> c
f g b a = g a b

-- Sample binary trees for testing
arv1 :: Arv Int
arv1 = F  -- Empty tree (should be balanced)

arv2 :: Arv Int
arv2 = N 1 F F  -- Single node tree (should be balanced)

arv3 :: Arv Int
arv3 = N 1 (N 2 F F) (N 3 F F)  -- Balanced tree with 3 nodes

arv4 :: Arv Int
arv4 = N 1 (N 2 (N 4 (N 4 F F) F) F) (N 3 F F)  -- Unbalanced tree

arv5 :: Arv Int
arv5 = N 1 (N 2 F (N 4 F F)) (N 3 F F)  -- Balanced tree

-- Tests in GHCi
main :: IO ()
main = do
    print $ equilibrada arv1  -- Should print True
    print $ equilibrada arv2  -- Should print True
    print $ equilibrada arv3  -- Should print True
    print $ equilibrada arv4  -- Should print False
    print $ equilibrada arv5  -- Should print True
