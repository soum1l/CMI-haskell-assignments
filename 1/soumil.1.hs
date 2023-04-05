-- Problem 1
nextSquare :: Integer -> Integer
nextSquare = (^2) . ceiling . sqrt . fromIntegral

-- Problem 2
previousCube :: Integer -> Integer
previousCube x = loop 0
  where
  loop :: Integer -> Integer
  loop n | cube n >= x  = cube (n-1)
         | otherwise    = loop (n+1)
    where cube n = n^3

-- Problem 3
nextPalin :: Integer -> Integer
nextPalin x = if x == (revNum x) then x else nextPalin (x+1)
  where
  revNum :: Integer -> Integer
  revNum  z | z >= 0    = revLoop 0 z
            | otherwise = 0

  revLoop :: Integer -> Integer -> Integer
  revLoop a 0 = a
  revLoop a b = revLoop (a*10 + r) (q)
    where (q,r) = divMod b 10

-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn i l = [ x | x <- primeSieve [2..l], x >= i ]
  where
  primeSieve :: [Integer] -> [Integer]
  primeSieve [] = []
  primeSieve (h:xs) = h : primeSieve [ x | x <- xs, (x `mod` h) /= 0 ]

-- Problem 5
isPrime :: Integer -> Bool
isPrime x = [x] == primesIn x x

-- Problem 6
decToBin :: Integer -> Integer
decToBin 0 = 0
decToBin x = 10*(decToBin q) + r
  where (q,r) = divMod x 2

-- Problem 7
binToDec :: Integer -> Integer
binToDec 0 = 0
binToDec x = 2*(binToDec q) + r
  where (q,r) = divMod x 10
