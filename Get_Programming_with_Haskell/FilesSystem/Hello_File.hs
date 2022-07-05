import System.IO

main :: IO ()
main = do
  helloFile <- openFile "hello.txt" ReadMode
  firstLine <- hGetLine helloFile
  putStrLn firstLine
  secondLine <- hGetLine helloFile
  goodByeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodByeFile secondLine
  hClose goodByeFile
  putStrLn "Done!"
