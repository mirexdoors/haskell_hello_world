import System.Environment

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where charCount = length input
        wordCount = (length . words) input
        lineCount = (length . lines) input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) = unwords ["symbols: ", show cc, "words: ", show wc, "lines: ", show lc]

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- readFile fileName
  let summary = (countsText . getCounts) input
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
  putStrLn summary
