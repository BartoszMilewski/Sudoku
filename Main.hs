module Main where

type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Char

digits :: [Char]
digits = ['1'..'9']

blank :: Digit -> Bool
blank = (== '0')

solve :: Grid -> [Grid]
solve = search . choices

search :: Matrix [Digit] -> [Grid]
search cm 
  | not (safe pm) = []
  | complete pm = [extract pm]
  | otherwise = concat (map search (expand pm))
  where pm = prune cm

choices :: Grid -> Matrix [Digit]
choices = map (map choice)
  where
    -- replace blank with a list of all digits
    -- replace digit with a singleton list
    choice :: Digit -> [Digit]
    choice d = if blank d then digits else [d]

expand :: Matrix [Digit] -> [Matrix [Digit]]
expand rows = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
    where
        (rows1, row : rows2) = break (any smallest) rows
        (row1, cs : row2)    = break smallest row
        smallest cs          = length cs == n
        n                    = minimum (counts rows)
        counts               = filter (/=1) . map length . concat


-- are all entries singletons?
complete :: Matrix [Digit] -> Bool
complete = all (all single)
  where
    single :: [a] -> Bool
    single [_] = True
    single _   = False

-- no duplicate singletons in rows, cols, boxs
safe :: Matrix [Digit] -> Bool
safe cm = all ok (rows cm) &&
          all ok (cols cm) &&
          all ok (boxs cm)
  where
    ok row = nodups [x | [x] <- row]
    nodups :: (Eq a) => [a] -> Bool
    nodups [] = True
    nodups (x : xs) = all (/= x) xs && nodups xs

-- extract rows, cols, and boxs
rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs : xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group

group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows

pruneBy f = f . map pruneRow . f

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove fixed) row
    where fixed = [d | [d] <- row]

remove :: [Digit] -> [Digit] -> [Digit]
remove ds [x] = [x]
remove ds xs  = filter (`notElem` ds) xs

extract :: Matrix [Digit] -> Grid
extract = map (map head)

--------------------

boardStr :: [String]
boardStr = [
   ".98 ... ...",
   "... .7. ...",
   "... .15 ..."
   ,
   "1.. ... ...",
   "... 2.. ..9",
   "... 9.6 .82"
   ,
   "... ... .3.",
   "5.1 ... ...",
   "... 4.. .2."]

-- replace dots with 0 and remove spaces
normalize :: String -> String
normalize = filter (/= ' ') . map (\c -> if c == '.' then '0' else c)

board :: Grid
board =  map normalize boardStr

printGrid :: Grid -> IO ()
printGrid g = mapM_ print3Rows (group g) >> putStrLn ""

print3Rows r = mapM_ printRow r >> putStrLn ""

printRow :: Row Char -> IO ()
printRow r = mapM_ print3 (group r) >> putStrLn ""

print3 :: [Char] -> IO ()
print3 t = mapM_ putDigit t >> putChar ' '
  where putDigit d = putChar d >> putChar ' '

main :: IO ()
main = mapM_ printGrid (solve board)
