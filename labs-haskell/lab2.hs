import Language.Haskell.TH (safe)
import Data.Char

--2.1)
myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand(xs)


--2.2)
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse i (x:xs) = x : i : intersperse i xs

--2.3)
mdc :: Integer -> Integer -> Integer
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

--2.4)
insert :: Ord a => a -> [a] -> [a]
insert i [] = [i]
insert i (x:xs) =
    if i < x then i : x : xs
    else x : insert i xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

--2.5)
minimum2 :: Ord a => [a] -> a
minimum2 [] = error "empty list"
minimum2 [x] = x
minimum2 (x:xs)
    | x < minimum2 xs = x
    | otherwise = minimum2 xs

delete2 :: Eq a => a -> [a] -> [a]
delete2 i (x:xs) = 
    if i == x then xs
    else x : delete2 i xs

ssort2 :: Ord a => [a] -> [a]
ssort2 [] = []
ssort2 xs = minimum2 xs : ssort2(delete2 (minimum2 xs) xs)

--2.6)
soma_100_quadrado :: Integer
soma_100_quadrado = sum [x^2 | x <- [1..100]]

--2.7)
aprox :: Int -> Double
aprox n = 4 * sum [((-1)^x) / fromIntegral (2 * x + 1) | x <- [0..n-1]]

aprox' :: Int -> Double
aprox' k = sqrt (12 * sum [((-1)^x) / fromIntegral ((x + 1)^2) | x <- [0..k-1]])

--2.8)
dotprod :: [Float] -> [Float] -> Float
dotprod xs ys = sum [x * y | (x,y) <- zip xs ys]

--2.9)
divprop :: Integer -> [Integer]
divprop n = [x | x <- [1..n-1], n `mod` x == 0]

--2.10)
perfeitos :: Integer -> [Integer]
perfeitos x = [i | i <- [1..x], sum (divprop i) == i ]

--2.11)
pitagoricos :: Integer -> [(Integer ,Integer ,Integer)]
pitagoricos x = [(a,b,c) | a <- [1..x], b <- [1..x], c <-[1..x], a^2+b^2 == c^2  ]

--2.12)
primo :: Integer -> Bool
primo x = divprop x == [1]

--2.13)
mersennes :: [Int]
mersennes = [ i | i <- [1..30], primo(2^i - 1)]

--2.15)
cifrar :: Int -> String -> String
cifrar n xs = [cifrarAux n x | x <- xs]
    where cifrarAux n x
            | (x >= 'a' && x <= 'z') = chr (ord 'a' + (ord x - ord 'a' + n) `mod` 26)
            | (x >= 'A' && x <= 'Z') = chr (ord 'A' + (ord x - ord 'A' + n) `mod` 26)
            | otherwise = x

--2.20)
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose xss = [head xs | xs <- xss] : transpose [tail xs | xs <- xss]

--2.21)
algarismos :: Int -> [Int]
algarismos n
    | n < 10 = [n]
    | otherwise = algarismos (n `div` 10) ++ [n `mod` 10]

