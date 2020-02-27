import Data.List

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode [x] = [(1, x)]
encode (x:xs) = if x == (snd . head . encode $ xs)
                then (1 + (fst . head . encode $ xs), x):(tail . encode $ xs)
                else (1, x):(encode xs)

data Encoded a = Single a | Multiple Int a
  deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified xs = map (\(len, val) -> if len == 1 then Single val else Multiple len val) (encode xs)

decodeModified :: [Encoded a] -> [[a]]
decodeModified = map decode
  where decode (Single a) = [a]
        decode (Multiple n a) = replicate n a

encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x:xs) = if x == decodeSymbol (head (encodeDirect xs))
                      then encodeSymbol (1 + (decodeNum . head . encodeDirect $ xs)) x:(tail . encodeDirect $ xs)
                      else Single x:encodeDirect xs
  where
    encodeSymbol 1 y = Single y
    encodeSymbol n y = Multiple n y
    decodeSymbol (Single y) = y
    decodeSymbol (Multiple _ y) = y
    decodeNum (Single _) = 1
    decodeNum (Multiple n _) = n

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

dupli' :: [a] -> [a]
dupli' xs = concat [[x,x] | x <- xs]

repli :: [a] -> Int -> [a]
repli xs n = concat [replicate n x | x <- xs]

repli' :: [a] -> Int -> [a]
repli' xs n = concat [take' n (repeat' x) | x <- xs]

repeat' :: a -> [a]
repeat' a = a:repeat' a

take' :: Int -> [a] -> [a]
take' n (x:xs)
  | n > 0 = x:take' (n-1) xs
  | otherwise = []

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEveryInner xs n 1
  where
    dropEveryInner [] _ _ = []
    dropEveryInner (x:xs) n counter
      | counter `mod` n == 0 = dropEveryInner xs n (counter + 1)
      | otherwise = x : dropEveryInner xs n (counter + 1)

split :: [a] -> Int -> [[a]]
split xs n = [take n xs, drop n xs]

slice :: [a] -> Int -> Int -> [a]
slice xs n m
  | n > 0 && n <=m = take (m - n + 1) (drop (n - 1) xs)

rotate :: [a] -> Int -> [a]
rotate (x:xs) n
  | n > 0 = rotate (xs ++ [x]) (n-1)
  | n == 0 = x:xs
  | n < 0 = rotate (x:xs) (length (x:xs) + n)

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs!!(n-1), take (n-1) xs ++ drop n xs)

main = do
  putStrLn "Exercises from: https://wiki.haskell.org/99_questions/11_to_20"

  putStrLn "-------E11"
  print $ encodeModified "aaaabccaadeeee"

  putStrLn "-------E12"
  print $ decodeModified
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']

  putStrLn "-------E13"
  print $ encodeDirect "aaaabccaadeeee"

  putStrLn "-------E14"
  print $ dupli [1, 2, 3]
  print $ dupli' [1, 2, 3]

  putStrLn "-------E15"
  print $ repli "abc" 3
  print $ repli' "abc" 3

  putStrLn "-------E16"
  print $ dropEvery "abcdefghik" 3

  putStrLn "-------E17"
  print $ split "abcdefghik" 3

  putStrLn "-------E18"
  print $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7

  putStrLn "-------E19"
  print $ rotate ['a','b','c','d','e','f','g','h'] 3
  print $ rotate ['a','b','c','d','e','f','g','h'] (-2)

  putStrLn "-------E20"
  print $ removeAt 2 "abcd"