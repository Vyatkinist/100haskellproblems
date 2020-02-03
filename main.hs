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