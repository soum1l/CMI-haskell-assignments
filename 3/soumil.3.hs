import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!) )

-- Question 1
selectLeader :: Int -> Int -> Int
selectLeader 1 k =  1
selectLeader n k = ( ( (selectLeader (n-1) (k)) + k-2 ) `mod` (n-1) ) + 2

-- Question 2
selectLeader' :: Int -> Int -> [[Int]]
selectLeader' n k =  go [1..n] 0 n 
  where--
  go [i] _  _ = [[i]]
  go  l  p  s = l : go (delete l p) ( ( p+k-1 ) `mod` (s-1) ) (s-1) 
    where--
    delete :: [Int] -> Int -> [Int]
    delete [    ] _ = []
    delete (x:xs) 0 = xs
    delete (x:xs) p = x : delete xs (p-1)

-- Question 3
cf :: Rational -> [Integer]
cf r  | rem == 0  = [quot]
      | otherwise = quot : cf ( (denominator r) % (rem) )
  where (quot, rem) = divMod (numerator r) (denominator r)

computeRat :: [Integer] -> Rational
computeRat  [i]   = (i % 1)
computeRat (i:is) = (i % 1) + reciproc ( computeRat is ) 
  where-- 
  reciproc :: Rational -> Rational
  reciproc r = (denominator r) % (numerator r)

-- Question 4
root6 :: Double 
root6 = sqrt 6 

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxRoot6 :: Double -> Rational
approxRoot6 eps = head [ r | r <- approxListRoot6, abs ( evalRat (r) - root6 ) < eps ] 
  where--
  cfRoot6 :: [Integer]
  cfRoot6 = 2:l where l = 2:4:l
  -------
  approxListRoot6 :: [Rational]
  approxListRoot6 = [ computeRat ( take n cfRoot6 ) | n <- [1..] ]
  
-- Question 5 
mss :: [Int] -> Int
mss xs = maximum $ ( zipWith (-) (tail maxs) sums )
  where--
  maxs = scanr1 max sums
  sums = scanl (+) 0 (map (^3) xs)

-- Question 6
lps :: Eq a => [a] -> (Int, [a])
lps xs = lcsTab!(lx, lx)  
  where--
  -------
  lx   = length xs
  xArr = listArray (0, lx-1) (xs)
  --
  lcsTab = listArray ( (0, 0), (lx, lx) ) [ f i j | i <- [0..lx], j <- [0..lx] ]
    where--
    -------
    f i 0 = (0, [])
    f 0 j = (0, [])
    f i j | xi==rxj   = ( fst d + 1, xi : snd d )
          | otherwise =   longer u l      
      where--
      xi  = xArr!( i -1 )
      rxj = xArr!( lx-j )
      -------
      l   = lcsTab!( i-1,  j  )
      u   = lcsTab!( i  , j-1 )
      d   = lcsTab!( i-1, j-1 )
      -------
      longer :: (Int, [a]) -> (Int, [a]) -> (Int, [a])
      longer t t' | fst t > fst t' = t
                  | otherwise      = t'
