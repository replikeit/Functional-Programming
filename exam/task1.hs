import Data.ByteString(readFile, unpack)
import Data.Map
import System.Directory.Internal.Prelude (getArgs)

getBytesFromFile = Data.ByteString.readFile 

histHelper :: (Ord k, Num a) => [k] -> Map k a -> Map k a
histHelper [] map = map
histHelper (x:xs) map =
    histHelper xs $ insertWith (+) x 1 map

getHist :: (Ord k, Num a) => [k] -> Map k a
getHist arr = histHelper arr empty

main = do
    (path:[]) <- getArgs
    content <- getBytesFromFile path
    print $ getHist $ unpack content