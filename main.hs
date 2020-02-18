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

--isEq :: [a] -> Integer -> Integer -> Bool
--isEq xs start end = (elementAt xs start) == (elementAt xs end)

--takeHalf :: [a] -> [a]
--takeHalf xs = skip (myLength xs)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = asd xs 0 1
  where asd xs start end = xs!!start == xs!!end

foldExample :: Num a => [a] -> a
foldExample xs = foldl (+) 0 xs

main = do
  putStrLn "Exercises from: https://wiki.haskell.org/99_questions/1_to_10"

  putStrLn "E1: Last element"
  putStrLn . show $ myLast [1..4]
  putStrLn . show $ myLast ['a'..'z']

  putStrLn "E2: Element before last"
  putStrLn . show $ myButLast [1..3]
  putStrLn . show $ myButLast ['a'..'d']

  putStrLn "E3:"
  putStrLn . show $ elementAt [1,2,3] 2
  putStrLn . show $ elementAt "haskell" 5

  putStrLn "E4"
  putStrLn . show $ myLength [123, 456, 789]
  putStrLn . show $ myLength "Hello, world!"

  putStrLn . show $ myLength' [123, 456, 789]
  putStrLn . show $ myLength' "Hello, world!"

  putStrLn "E5"
  putStrLn . show $ myReverse' "A man, a plan, a canal, panama!"
  putStrLn . show $ myReverse' [1,2,3,4]
  
  putStrLn "E6"
  putStrLn . show $ isPalindrome [1,2,3]
  putStrLn . show $ isPalindrome "madamimadam"
  putStrLn . show $ isPalindrome [1,2,4,8,16,8,4,2,1]

  putStrLn . show $ fst $ splitAt 2 [1..9]