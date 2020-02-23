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
decodeModified = map (decode)
  where decode (Single a) = [a]
        decode (Multiple n a) = replicate n a

encodeDirect :: [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x:xs) = if x == (head (encodeDirect xs))

main = do
  putStrLn "Exercises from: https://wiki.haskell.org/99_questions/11_to_20"

  putStrLn "-------E11"
  putStrLn . show $ encodeModified "aaaabccaadeeee"

  putStrLn "-------E12"
  putStrLn . show $ decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']

  putStrLn . show $ encodeDirect "aaaabccaadeeee"