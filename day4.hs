import Data.List.Split

board_size = 5
type Board = [[Int]]

draw :: Int -> Board -> Board
draw i brd = [[if j == i then -1 else j | j <- row] | row <- brd]

draw_on_boards :: Int -> [Board] -> [Board]
draw_on_boards = map . draw

board_score :: Board -> Int
board_score = sum . map (foldr (\ s x -> x + (if s == -1 then 0 else s)) 0)

row_bingo :: Int -> Board -> Bool
row_bingo i brd = all (== -1) (brd !! i)

col_bingo :: Int -> Board -> Bool
col_bingo i = all ((== -1) . (!! i))

bingo :: Board -> Bool
bingo brd = any (\ i -> row_bingo i brd || col_bingo i brd) [0..board_size-1]

readNums :: Char -> String -> [Int]
readNums del = map (\ s -> read s :: Int) . filter (not . null) . splitWhen (== del)

anyBingos :: [Board] -> Maybe Int
anyBingos = foldr (\ brd -> maybe (if bingo brd then Just (board_score brd) else Nothing) Just) Nothing

part2filter :: [Board] -> [Int] -> Int
part2filter [brd] (i : is) =
  let brd' = draw i brd in
  if bingo brd' then i * (board_score brd') else part2filter [brd'] is
part2filter brds (i : is) =
  part2filter [brd | brd <- draw_on_boards i brds, not (bingo brd)] is

part1 =
  readNums ','
    <$> getLine >>= \ ns ->
  map (map (readNums ' '))
    <$> splitWhen null
    <$> tail
    <$> lines
    <$> getContents >>= \ boards ->
  putStrLn (show (foldl (\ r n -> draw_on_boards n <$> r >>= \ brds -> maybe (Right brds) (Left . (*n)) (anyBingos brds)) (Right boards) ns))

part2 =
  readNums ','
    <$> getLine >>= \ ns ->
  map (map (readNums ' '))
    <$> splitWhen null
    <$> tail
    <$> lines
    <$> getContents >>= \ boards ->
  putStrLn (show (part2filter boards ns))


main = part2
