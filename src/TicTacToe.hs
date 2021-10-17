import           Data.Char
import           Data.List
import           System.IO
import           Text.Read

size :: Int
size = 3

data Player = O | B | X
  deriving (Eq, Ord, Show, Read)

next :: Player -> Player
next O = X
next X = O
next B = B -- for completeness

type Grid = [[Player]]

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
 where
  os = length $ filter (== O) ps
  xs = length $ filter (== X) ps
  ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ diags)
 where
  line  = all (== p)
  rows  = g
  cols  = transpose g
  diags = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [ g !! n !! n | n <- [0 .. size - 1] ]

terminal :: Grid -> Bool
terminal g = wins O g || wins X g || full g

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . interleave bar . map showRow
  where bar = concat $ replicate ((size * 4) - 1) "-"

showRow :: [Player] -> String
showRow = concat . interleave "|" . map showPlayer

showPlayer :: Player -> String
showPlayer B = "   "
showPlayer p = " " ++ show p ++ " "

interleave :: a -> [a] -> [a]
interleave x []       = []
interleave x [y     ] = [y]
interleave x (y : ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs, B : ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat prompt

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | otherwise = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        run' g p
      [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Opponent = Human | Machine

instance Read Opponent where
  readsPrec _ "H" = [(Human, "")]
  readsPrec _ "M" = [(Machine, "")]
  readsPrec _ _   = []

selectOpponent :: IO Opponent
selectOpponent = do
  putStr "Select Human (H) or Machine (M) opponent: "
  o <- getLine
  case (readMaybe o) of
    Just o  -> return o
    Nothing -> do
      putStrLn "ERROR: Invalid option"
      selectOpponent

-- Runs tic-tac-toe in Human vs Human mode
playHuman :: IO ()
playHuman = run empty O

data Tree a = Node a [Tree a]
  deriving Show

moves :: Grid -> Player -> [Grid]
moves g p = if terminal g
  then []
  else concat [ move g i p | i <- [0 .. ((size ^ 2) - 1)] ]

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [ gametree g' (next p) | g' <- moves g p ]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _ ) = Node x []
prune n (Node x ts) = Node x [ prune (n - 1) t | t <- ts ]

depth :: Int
depth = 9

moveTo :: Tree Grid -> Grid -> Tree Grid
(Node _ ts) `moveTo` g = head [ t | t@(Node g' _) <- ts, g' == g ]

-- Labels nodes with the sub-tree winning player using the Minimax algorithm
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g []) | wins O g  = Node (g, O) []
                    | wins X g  = Node (g, X) []
                    | otherwise = Node (g, B) []
minimax (Node g ts) = case turn g of
  O -> Node (g, minimum ps) ts'
  X -> Node (g, maximum ps) ts'
 where
  ts' = map minimax ts
  ps  = [ p | Node (_, p) _ <- ts' ]

bestmove :: Tree Grid -> Player -> Grid
bestmove t@(Node g _) p = head [ g' | Node (g', p') _ <- ts, p' == best ]
  where Node (_, best) ts = minimax (prune depth t)

playMachine :: IO ()
playMachine = do
  human <- selectPlayer
  play (gametree empty O) O human

play :: Tree Grid -> Player -> Player -> IO ()
play t@(Node g _) p h = do
  cls
  goto (1, 1)
  putGrid g
  play' t p h

play' :: Tree Grid -> Player -> Player -> IO ()
play' t@(Node g _) p h
  | wins h g = putStrLn "Human wins!\n"
  | wins (next h) g = putStrLn "Machine wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == h = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        play' t p h
      [g'] -> play (t `moveTo` g') (next p) h
  | otherwise = do
    putStr "Player X is thinking... "
    let t' = t `moveTo` (bestmove t p)
    (play $! t') (next p) h

selectPlayer :: IO Player
selectPlayer = do
  putStr "Select whether to play first (O) or second (X): "
  p <- getLine
  case (readMaybe p) of
    Just p  -> return p
    Nothing -> do
      putStrLn "ERROR: Invalid option"
      selectPlayer

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  opponent <- selectOpponent
  case opponent of
    Human   -> playHuman
    Machine -> playMachine
