import Control.Monad
import Data.List (nub, sort)

main :: IO ()
main = do
    m:n:_ <- map read . words <$> getLine
    ms <- replicateM m readLn
    ns <- replicateM n readLn
    let boxes = nub . sort $ (*) <$> ns <*> ns
    forM_ ms $ print . solve boxes

solve :: [Int] -> Int -> Int
solve boxes n = subtract n . head . dropWhile (< n) $ boxes

