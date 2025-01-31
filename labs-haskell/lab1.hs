import Language.Haskell.TH (safe)
-- 1.1)
testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = (a + b > c) && (a + c > b) && (b + c > a)

-- 1.2)
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt(s * (s-a) * (s-b) * (s-c)) where s = (a + b + c) / 2

-- 1.3)

metades :: [a] -> ([a], [a])
metades xs = (take half xs, drop half xs) where half = (1 + length xs) `div` 2

-- 1.4)

lastList :: [a] -> a
lastList xs = head (reverse xs)

ultimo :: [a] -> a
ultimo [x] = x
ultimo (_ : xs) = ultimo xs

removeLastList :: [a] -> [a]
removeLastList xs = take (length xs - 1) xs

-- 1.5)

binom :: Integer -> Integer -> Integer
binom n k = product [1..n] `div` (product [1..k] * product [1..t]) where t = n-k

binom' :: Integer -> Integer -> Integer
binom' n k = 
    if k < (n-k)
        then product [(n-k+1)..n] `div` product [1..k]
    else
        product [(k+1)..n] `div` product [1..(n-k)]

-- 1.6)

raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = ((-b + sqrt t)/(2*a), (-b - sqrt t)/(2*a)) where t = b^2 - 4 * a * c

--1.7
--a) [Char]
--b) (Char, Char, Char)
--c) [(Bool, Char)]
--d) ([Bool], [Char])
--e) [[a] -> [a]]
--f) [Bool -> Bool]

-- 1.8)
--a) segundo :: [a] -> a
--b) trocar :: (a,b) -> (b,a)
--c) par :: a -> b -> (a,b)
--d) dobro :: Num a => a -> a
--e) metade :: Fractional a => a -> a
--f) minuscula :: Char -> Bool
--g) intervalo :: Ord a => a -> a -> a -> Bool
--h) palindromo :: Eq a => [a] -> Bool
--i) twice :: (a -> a) -> a -> a

--1.9)

classificaCond :: Int -> String
classificaCond nota = 
    if nota <= 9 then "reprovado"
    else if nota <= 12 then "suficiente"
    else if nota <= 15 then "bom"
    else if nota <= 18 then "muito bom"
    else if nota <= 20 then "muito bom com distinção"
    else "invalido"

classificaGuardas :: Int -> String
classificaGuardas nota 
    | nota <= 9 = "reprovado"
    | nota <= 12 = "suficiente"
    | nota <= 15 = "bom"
    | nota <= 18 = "muito bom"
    | nota <= 20 = "muito bom com distinção"
    | otherwise = "invalido"

--1.10)

classifica :: Float -> Float -> String
classifica peso altura
    | imc < 18.5 = "baixo peso"
    | imc < 25 = "peso normal"
    | imc < 30 = "excesso de peso"
    | otherwise = "obesidade"
    where imc = peso/(altura^2)

--1.11)

max3, min3 :: Ord a => a -> a -> a -> a
max3 x y z 
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | otherwise = z
        
min3 x y z 
    | x <= y && x <= z = x
    | y <= x && y <= z = y
    | otherwise = z

max3', min3' :: Ord a => a -> a -> a -> a
max3' x y z = max x (max y z)

min3' x y z = min x (min y z)

--1.12)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

--1.13)

safetail1 :: [a] -> [a]
safetail1 xs = 
    if null xs then []
    else tail xs

safetail2 :: [a] -> [a]
safetail2 xs
    | null xs = []
    | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

--1.14)

curta :: [a] -> Bool
curta xs = length xs <= 2

curtaGuardas :: [a] -> Bool
curtaGuardas xs 
    | null xs = True
    | length xs == 1 = True
    | length xs == 2 = True
    | otherwise = False

curtaPadroes :: [a] -> Bool
curtaPadroes [] = True
curtaPadroes [_] = True
curtaPadroes [_,_] = True
curtaPadroes _ = False

--1.15)

mediana :: Ord a => a -> a -> a -> a
mediana a b c
    | (b >= a && c <= a) || (b <= a && c >= a) = a
    | (a >= b && c <= b) || (a <= b && c >= b) = b
    | otherwise = c

mediana2 :: (Num a, Ord a) => a-> a -> a -> a
mediana2 a b c = soma - min a (min b c) - max a (max b c) where soma = a+b+c



