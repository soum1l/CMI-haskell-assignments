import Data.List

-- Question 1

shrink :: Eq a => [a] -> [a]
shrink l = shrink' id l

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' f [ ] = [ ]
shrink' f [x] = [x]
shrink' f (x:x':xs)
  | f(x)==f(x') = shrink' f (x:xs)
  | otherwise   = x:shrink' f (x':xs)

myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]

-- Question 3

squares :: [(Int,Int)]
squares = [(x,y) | x <- [0..7], y <- [0..7]]

knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove (x0,y0) n
  | n <0      = []
  | n==0      = [ (x0,y0) ]
  | otherwise = (shrink . sort . concat . map knightMoveStep) (knightMove (x0,y0) (n-1))
  where -------
  
  knightMoveStep :: (Int,Int) -> [(Int,Int)]
  knightMoveStep (x0,y0) = [ (x,y) | (x,y) <- squares, x/=x0, y/=y0, abs(x-x0)+abs(y-y0) == 3 ] 
  
-- Question 4 

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' (x0,y0) n 
  | n<=0      = knightMove (x0,y0) n
  | otherwise = (shrink . sort) (knightMove (x0,y0) (n) ++ knightMove' (x0,y0) (n-1))

-- Question 5

cartUnion :: [[a]] -> [[a]] -> [[a]]
cartUnion ls ls' = [ l ++ l' | l <- ls, l' <- ls' ] 

tuples :: [[a]] -> [[a]]
tuples [] = [[]]
tuples (l:ls) = cartUnion (map (\x -> [x]) l) (tuples ls)

-- Question 6

injTuples :: Eq a => [[a]] -> [[a]]
injTuples ls = (filter isInj) (tuples ls)

isInj :: Eq a => [a] -> Bool
isInj [] = True
isInj (x:xs) = (not . elem x) (xs) && isInj xs

-- Question 7

pg, sg, sf, pf, c :: [String]
pg = ["LeBron", "Russ", "Rondo", "Nunn"]
sg = ["Monk", "Baze", "Bradley", "Reaves", "THT", "Ellington", "LeBron"]
sf = ["LeBron", "Melo", "Baze", "Ariza"]
pf = ["LeBron", "AD", "Melo"]
c  = ["AD", "DJ", "Dwight"]

traditionalLineUps :: [[String]]
traditionalLineUps = injTuples [pg, sg, sf, pf, c]

lineUps :: Eq a => [(Int, [a])] -> [[a]]
lineUps ts = (filter isInj) (foldr cartUnion [[]] (map (subsetSize) ts))
  where -------
  
  subsetSize :: (Int,[a]) -> [[a]]
  subsetSize (0, s ) = [[]]
  subsetSize (n,[ ]) = [  ]
  subsetSize (n,(x:xs)) = map (x:) (subsetSize (n-1,xs)) ++ (subsetSize (n,xs))
  
fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]