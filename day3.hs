module Main where

bs2i :: String -> Int
bs2i = foldl (\ i c -> case c of { '0' -> 2 * i; '1' -> 2 * i + 1 }) 0

count_bits :: Int -> String -> [(Int, Int)] -> [(Int, Int)]
count_bits inc s bcs = [case c of { '0' -> (b0 + inc, b1); '1' -> (b0, b1 + inc) } | (c, (b0, b1)) <- zip s bcs]

select :: Char -> [(Int, Int)] -> [(String, String)] -> String
select c bcs [(sp, s)] = reverse sp ++ s
select c ((b0, b1) : bcs) ls =
  uncurry (select c) $
    foldr
      (\ (lp, (lh : lt)) (bcs, ls) ->
           if   (lh == c) == (b0 > b1)
           then (bcs, (lh : lp, lt) : ls)
           else (count_bits (-1) lt bcs, ls))
      (bcs, []) ls

main =
  lines <$> getContents >>= \ ls ->
  putStrLn (show (product [bs2i (select c (foldr (count_bits 1) (replicate (length (head ls)) (0, 0)) ls) [("", l) | l <- ls]) | c <- ['0', '1']]))
