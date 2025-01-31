type Species = (String, Int)
type Zoo = [Species]

isEndangered :: Species -> Bool
isEndangered (s, i) = i <= 100

updateSpecies :: Species -> Int -> Species
updateSpecies (s, i) babies = (s, i+babies)

filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] _ = []
filterSpecies (z:zs) funct = if funct z 
                            then z : filterSpecies zs funct
                            else filterSpecies zs funct

countAnimals :: Zoo -> Int
countAnimals z = foldl (\acc (a,b) -> acc + b) 0 z

substring :: (Integral a) => String -> a -> a -> String
substring s a b = [c | c <- (drop (fromIntegral a) (take (fromIntegral b + 1) s)) ]


hasSubstr :: String -> String -> Bool
hasSubstr a b = if take (length b) a == b then True 
                else if null a then False
                else hasSubstr (tail a) b

sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo)
sortSpeciesWithSubstr z s = ((filter (\(str,inteiro) -> (hasSubstr str s)) z) , (filter (\(str,inteiro) -> (not $ hasSubstr str s)) z))

rabbits :: (Integral a) => [a]
rabbits = 2 : 3 : [a+b | (a,b) <- zip rabbits (tail rabbits)]

rabbitYears :: (Integral a) => a -> Int
rabbitYears i = length (takeWhile (< fromIntegral i) rabbits)

data Dendrogram = Leaf String | Node Dendrogram Int Dendrogram
myDendro :: Dendrogram
myDendro = Node (Node (Leaf "dog") 3 (Leaf "cat")) 5 (Leaf "octopus")

dendroWidth :: Dendrogram -> Int
dendroWidth (Leaf _) = 0
dendroWidth (Node esq i dir) = 2 * i +  (dendroWidthAux esq) + (dendroWidthAux dir)

dendroWidthAux :: Dendrogram -> Int
dendroWidthAux (Leaf _) = 0
dendroWidthAux (Node esq i dir) = i +  (dendroWidthAux esq) + (dendroWidthAux dir)

dendroInBounds :: Dendrogram -> Int -> [String]
dendroInBounds d i = dendroInBounds' d i True

dendroInBounds' :: Dendrogram -> Int -> Bool -> [String]
dendroInBounds' (Leaf str) maxi _
    | maxi >= 0 = [str]
    | otherwise = []
dendroInBounds' (Node esq i dir) maxi flag
    | maxi < 0 = []
    | otherwise = if flag then (dendroInBounds' esq (maxi - i) True) ++ (dendroInBounds' dir (maxi + i) False)
                else (dendroInBounds' esq (maxi + i) True) ++ (dendroInBounds' dir (maxi - i) False)
