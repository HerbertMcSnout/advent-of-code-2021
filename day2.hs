module Main where

lineToMove ('f' : 'o' : 'r' : 'w' : 'a' : 'r' : 'd' : ' ' : s) (h, d, aim) =
  let x = read s :: Int in
    (h + x, d + aim * x, aim)
lineToMove ('d' : 'o' : 'w' : 'n' : ' ' : s) (h, d, aim) =
  let x = read s :: Int in
    (h, d, aim + x)
lineToMove ('u' : 'p' : ' ' : s) (h, d, aim) =
  let x = read s :: Int in
    (h, d, aim - x)

main =
  getContents >>= \ cs ->
  let (h, d, aim) = foldl (flip lineToMove) (0, 0, 0) (lines cs) in
    putStrLn (show h ++ ", " ++ show d ++ ", " ++ show aim)
