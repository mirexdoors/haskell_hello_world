import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where safeInt = int `mod` 255


intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]


main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- return imageFile
    let glitchedFileName = mconcat["glitched_", fileName]
    BC.writeFile glitchedFileName glitched
    putStrLn "Done!"
