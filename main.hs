myLast :: [a] -> a
myLast (x:[]) = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast (x:[]) = error "A list has to contain at least two elements."
myButLast (x1:x2:[]) = x2
myButLast (_:x2:xs) = myButLast (x2:xs)

elementAt :: [a] -> Integer -> a
elementAt [] n = error "No element has been found."
elementAt (x:xs) n
  | n == 1 = x
  | otherwise = elementAt xs (n - 1)

myLength :: [a] -> Integer
myLength xs = foldl (\acc x -> acc + 1) 0 xs

myLength' :: [a] -> Integer
myLength' [] = 0
myLength' [x] = 1
myLength' (x:xs) = 1 + myLength' xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (\acc x -> x:acc) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = foldl (\acc x -> if fst x == snd x then acc else False) True (zipHalves xs)
  where zipHalves xs = zip (fstHalf xs) (reverse . sndHalf $ xs)
        splitAtHalf = splitAt (length xs `div` 2)
        fstHalf = fst . splitAtHalf
        sndHalf = snd . splitAtHalf

foldExample :: Num a => [a] -> a
foldExample xs = foldl (+) 0 xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

compress :: Eq a => [a] -> [a]
compress xs = foldl (\acc x -> if (last acc) == x then acc else acc ++ [x]) [head xs] (tail xs)

main = do
  putStrLn "Exercises from: https://wiki.haskell.org/99_questions/1_to_10"

  putStrLn "-------E1: Last element"
  putStrLn . show $ myLast [1..4]
  putStrLn . show $ myLast ['a'..'z']

  putStrLn "-------E2: Element before last"
  putStrLn . show $ myButLast [1..3]
  putStrLn . show $ myButLast ['a'..'d']

  putStrLn "-------E3:"
  putStrLn . show $ elementAt [1,2,3] 2
  putStrLn . show $ elementAt "haskell" 5

  putStrLn "-------E4"
  putStrLn . show $ myLength [123, 456, 789]
  putStrLn . show $ myLength "Hello, world!"

  putStrLn . show $ myLength' [123, 456, 789]
  putStrLn . show $ myLength' "Hello, world!"

  putStrLn "-------E5"
  putStrLn . show $ myReverse' "A man, a plan, a canal, panama!"
  putStrLn . show $ myReverse' [1,2,3,4]
  
  putStrLn "-------E6"
  putStrLn . show $ isPalindrome [1,2,3]
  putStrLn . show $ isPalindrome "madamimadam"
  putStrLn . show $ isPalindrome [1,2,4,8,16,8,4,2,1]

  putStrLn "-------E7"
  putStrLn . show $ flatten (Elem 5)
  putStrLn . show $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
  putStrLn $ flatten (List [])

  putStrLn "-------E8"
  putStrLn . show $ compress "aaaabccaadeeee"
