module Main where

group :: Int -> [a] -> [[a]]
group n as = take (length as - n + 1) (h as) where
  h (a : as) = take n (a : as) : group n as
  h [] = []

main =
  getContents >>= \ cs ->
  let ls = lines cs
      ns = [read x :: Int | x <- ls]
      ss = [a + b + c | [a, b, c] <- group 3 ns]
      incs = foldr (\ line next last -> (if line > last then 1 else 0) + next line) (const 0) ss 0
  in
  putStrLn (show (incs - 1))
