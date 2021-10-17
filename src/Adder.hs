import           System.IO                      ( hFlush
                                                , stdout
                                                )

-- Immediately displays given text
-- Solves buffering issues: https://stackoverflow.com/a/13190872
prompt :: String -> IO ()
prompt text = do
  putStr text
  hFlush stdout

-- Reads a number from stdin
readNum :: IO Int
readNum = do
  x <- getLine
  return (read x :: Int)

-- Reads n numbers from stdin
adder :: Int -> IO [Int]
adder n = sequence $ replicate n readNum

main :: IO ()
main = do
  prompt "How many numbers? "
  n  <- readNum
  ns <- adder n
  let total = sum ns
  putStrLn ("The total is " ++ show total)
