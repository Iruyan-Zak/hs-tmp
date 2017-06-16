module InfoTheorem where

import Data.List (sort, sortOn)
import Control.Monad.Free

data Symbol = Symbol
                { prob :: Float
                , label :: Free [] Int
                , code :: String
                } deriving (Show, Ord, Eq)


toSymbols :: [Float] -> [Symbol]
toSymbols = imap $ \i p -> Symbol p (Pure i) ""


mergeSymbol :: Symbol -> Symbol -> Symbol
mergeSymbol (Symbol p1 n1 c1) (Symbol p2 n2 c2) = Symbol (p1+p2) (Free [n1, n2]) (c1 ++ c2)


log2 :: Floating a => a -> a
log2 = logBase 2


infoContent :: Float -> Float
infoContent p = -p * log2 p


entropy :: [Float] -> Float
entropy = sum . map infoContent


meanLength :: [(Float, Int)] -> Float
meanLength = sum . map (\(p, l) -> p * fromIntegral l)


huffman :: [Float] -> [String]
huffman = map snd . sortOn fst . huffmanExpand . huffmanFold . toSymbols
    where
        huffmanFold :: [Symbol] -> Symbol
        huffmanFold [] = undefined
        huffmanFold [x] = x
        huffmanFold (a:b:xs) = huffmanFold . sort $ (mergeSymbol a b):xs

        huffmanExpand :: Symbol -> [(Int, String)]
        huffmanExpand (Symbol _ (Pure l) c) = [(l, c)]
        huffmanExpand (Symbol _ (Free [a, b]) c) =
            huffmanExpand (Symbol 0 a (c++"0")) ++ huffmanExpand (Symbol 0 b (c++"1"))
        huffmanExpand _ = undefined


imap :: (Int -> b -> c) -> [b] -> [c]
imap f = zipWith f [0..]

