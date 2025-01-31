import Language.Haskell.TH (safe, cApi)
import Data.List

-- 3.1)
-- map f (filter p xs)

--3.2)
dec2int :: [Int] -> Int
dec2int xs = foldl (\acc x -> acc*10 + x) 0 xs

--3.3)
zipwith2 ::  (a -> b -> c) -> [a] -> [b] -> [c]
zipwith2 _ _ [] = []
zipwith2 _ [] _ = []
zipwith2 f (x:xs) (y:ys) = f x y : zipwith2 f xs ys

--3.4)
isort2 :: Ord a => [a] -> [a]
isort2 = foldr insert [] 

--3.7)
maismais :: [a] -> [a] -> [a]
maismais xs ys = foldr (:) ys xs

concat2 :: [[a]] -> [a]
concat2 xs = foldr (++) [] xs