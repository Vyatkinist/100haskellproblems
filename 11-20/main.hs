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

  