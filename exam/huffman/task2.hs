
import Huffman(encode, decode, getHist, histToHuffmanTree, treeToBitsMapDfs)
import Data.Map (elems, keys)
import System.Environment (getArgs)
import Data.ByteString as BS (ByteString, pack, readFile, unpack, writeFile)
import Data.Bits (clearBit, setBit, shiftL, testBit, zeroBits)
import Data.Word (Word8)

separateBy :: Int -> [a] -> [[a]]
separateBy n xs = separateBy' n xs []
  where
    separateBy' _ [] acc = reverse acc
    separateBy' n xs acc = separateBy' n (drop n xs) (take n xs:acc)

byteToBits :: Word8 -> [Bool]
byteToBits b = Prelude.map (testBit b) (reverse [0 .. 7])

bitsToByte :: [Bool] -> Word8
bitsToByte = foldr setter zeroBits . reverse
  where
    setter f c = (if f then setBit else clearBit) (shiftL c 1) 0

bitsToWords :: [Bool] -> [Word8]
bitsToWords = fmap bitsToByte . separateBy 8

bitsToBS :: [Bool] -> ByteString
bitsToBS = BS.pack . bitsToWords

compress :: FilePath -> FilePath -> IO ()
compress infile outfile = do
  bytes <- fmap unpack (BS.readFile infile)
  let encodings = treeToBitsMapDfs $ histToHuffmanTree $ getHist bytes
  let encodedBytes = bitsToBS $ encode encodings bytes
  BS.writeFile outfile encodedBytes

compressProcess :: [[Char]] -> IO ()
compressProcess | ["-c", filename, outfile] = compress filename outfile
                | otherwise = error "bad args!"

main :: IO ()
main = do
    args <- getArgs
    compressProcess args 