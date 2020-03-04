import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = foldr (\x acc -> tailFib acc:acc) [] [0..]
    where 
        tailFib :: [Integer] -> Integer
        tailFib [] = 0
        tailFib [0] = 1
        tailFib xs = last xs + xs!!(length xs - 1)

main :: IO()
main = do
    putStrLn . show $ "123"
    print $ take 10 fibs1
    print $ take 10 fibs2
