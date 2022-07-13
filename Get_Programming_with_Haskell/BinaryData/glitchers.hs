import System.Environment
import System.Random
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where safeInt = int `mod` 255


intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc chV bytes = mconcat [before, newChar, after]
    where (before, rest) = BC.splitAt loc bytes
          after = BC.drop 1 rest
          newChar = intToBC chV

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    location <- randomRIO (1, bytesLength)
    chV <- randomRIO (0, 255)
    return (replaceByte location chV bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
    where (before, rest) = BC.splitAt start bytes
          (target, after) = BC.splitAt size rest
          changed = BC.sort target

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (sortSection start sectionSize bytes)

-- TODO: randomReverseByte:  reverse random bytes section
main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- foldM (\bytes func -> func bytes) imageFile
                      [randomReplaceByte, randomSortSection, randomReplaceByte, randomSortSection, randomReplaceByte]
    let glitchedFileName = mconcat["glitched_", fileName]
    BC.writeFile glitchedFileName glitched
    putStrLn "Done!"
