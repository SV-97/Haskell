{-
Given an alphabet of size N and a randomly generated string S_1 of length L,
what is the average hamming distance from S_1 to another randomly generated
string of length L.
-}

hamming :: Eq a => [a] -> [a] -> Int
hamming = hamminghelper 0
  where
    hamminghelper n (a:as) (b:bs)
      | a == b = hamminghelper n as bs
      | otherwise = hamminghelper (n+1) as bs
    hamminghelper n [] [] = n

strings :: [a] -> Int -> [[a]]
strings alphabet 1 = map (:[]) alphabet
strings alphabet length = concatMap f $ strings alphabet (length - 1)
  where f substring = [x : substring | x <- alphabet]

fac :: Int -> Int
fac 1 = 1
fac 0 = 1
fac n = n * fac (n-1)

binom :: Int -> Int -> Int
binom n k = fac n `div` (fac k * fac (n - k))

alphabet = "abcd"
n = length alphabet

seq' = "aaaaaa"
l = length seq'
strings' = strings alphabet l

d = sum [binom l j * (n - 1)^j * j | j <- [1..l]]
md = fromIntegral d / (fromIntegral n)**(fromIntegral l) -- = L*(N-1)/N

main = do
  print $ n ^ l
  print d
  print $ sum $ map (hamming seq') strings'
  print md
  let f n = length $ filter (\x -> hamming x seq' == n) strings'
    in mapM_ (\n -> do
      putStr $ show n ++ ": "
      (print.f) n) [0..l]