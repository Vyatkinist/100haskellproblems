import Data.List

insertAt :: a -> [a] -> Int -> [a]
insertAt c xs n = before ++ [c] ++ after
  where
    (before, after) = splitAt (n-1) xs

insertAt' :: a -> [a] -> Int -> [a]
insertAt' c xs n = let (before, after) = splitAt (n-1) xs in before ++ c:after

range :: Int -> Int -> [Int]
range m n
  | m < n = m:range (m+1) n
  | m == n = [m]
  | m > n = m:range (m-1) n

main = do
  putStrLn "Exercises from: https://wiki.haskell.org/99_questions/11_to_20"

  putStrLn "-------E21"
  print $ insertAt 'X' "abcd" 2
  print $ insertAt' 'X' "abcd" 2
  
  putStrLn "-------E22"
  print $ range 4 9
  print $ range 9 4