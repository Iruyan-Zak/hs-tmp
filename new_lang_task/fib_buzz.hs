main :: IO ()
main = mapM_ (putStrLn . fizzbuzz 3 5) . take 100 $ fibonacci 0 1

fizzbuzz :: Integer -> Integer -> Integer -> String
fizzbuzz a b = do
    mulA <- (==0) . (`mod` a)
    mulB <- (==0) . (`mod` b)
    case (mulA, mulB) of
      (True, True) -> const "fizzbuzz"
      (True, False) -> const "fizz"
      (False, True) -> const "buzz"
      _ -> show

fibonacci :: Integer -> Integer -> [Integer]
fibonacci a0 a1 = a0 : fibonacci a1 (a0 + a1)

