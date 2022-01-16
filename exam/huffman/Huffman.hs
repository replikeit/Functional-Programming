{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Huffman
  ( 
    HuffmanTree,
    HuffmanBitsMap,
    getHist,
    histToHuffmanTree,
    huffmanTreeProcess,
    treeToBitsMapDfs,
    encode,
    decode
  )
where

import Data.Function (on)
import Data.List (insertBy)
import Data.Map (Map, assocs, singleton, union, (!), empty, insertWith)

data HuffmanTree a = Fork (HuffmanTree a) (HuffmanTree a) | Leaf (a, Integer)
type HuffmanBitsMap a = Map a [Bool]

encode :: (Foldable t, Ord a) => HuffmanBitsMap a -> t a -> [Bool]
encode encodings bytes = concatMap (encodings !) bytes

decode :: HuffmanTree a -> [Bool] -> [a]
decode tree = reverse . decoder []
  where
    decoder d [] = d
    decoder d codes = let (curr, r) = decodeSymbol tree codes in decoder (curr : d) r
      where
        decodeSymbol :: HuffmanTree a -> [Bool] -> (a, [Bool])
        decodeSymbol (Leaf (v, _)) codes = (v, codes)
        decodeSymbol (Fork a b) (c:codes) =
          let subTree = if c then a else b 
          in decodeSymbol subTree codes

getHist :: (Ord k, Num a) => [k] -> Map k a
getHist arr = histHelper arr empty
    where 
        histHelper [] map = map
        histHelper (x:xs) map = histHelper xs $ insertWith (+) x 1 map
        
histToHuffmanTree :: Ord a => Map a Integer -> HuffmanTree a
histToHuffmanTree frequencies = huffmanTreeProcess (map Leaf (assocs frequencies))

huffmanTreeProcess :: Ord a => [HuffmanTree a] -> HuffmanTree a
huffmanTreeProcess [tree] = tree
huffmanTreeProcess (l:r:xs) =
  let n = Fork l r in huffmanTreeProcess $ insertBy (compare `on` getWeight) n xs
    where 
        getWeight (Fork a b) = getWeight a + getWeight b
        getWeight (Leaf (v, w)) = w

treeToBitsMapDfs :: Ord a => HuffmanTree a -> HuffmanBitsMap a
treeToBitsMapDfs = treeToBitsMapDfs' []
  where
    treeToBitsMapDfs' symState (Leaf (v, w)) = singleton v (reverse symState)
    treeToBitsMapDfs' symState (Fork l r) = union (treeToBitsMapDfs' (True:symState) l) (treeToBitsMapDfs' (False:symState) r)

