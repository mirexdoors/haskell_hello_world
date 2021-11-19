module Main where

helloText :: [Char] -> String
helloText name = "Hi, " ++ name ++ "\n"

main :: IO ()
main = do
  putStrLn $ "What is your name?"
  putStr "Name: "
  name <- getLine
  if ((length name) > 0)
    then putStr $ helloText name
    else main

