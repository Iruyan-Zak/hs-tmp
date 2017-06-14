import Data.List (group, sort)
import Control.Arrow


data Grade = A | B | C | F deriving (Show, Eq, Ord, Enum)


judge :: Int -> Maybe Grade
judge score
    | score < 0     = Nothing
    | score > 100   = Nothing
    | score < 60    = Just F
    | score < 70    = Just C
    | score < 80    = Just B
    | otherwise     = Just A


crawl :: [Grade] -> IO [Grade]
crawl gs = do
    putStr "Input score: "
    x <- judge <$> readLn
    case x of
        Just g -> do
            print g
            crawl $ g:gs
        Nothing  -> return gs


main :: IO ()
main = print . map (subtract 1 <$>) . runlength . sort . ([A .. F] ++) =<< crawl []


runlength :: Eq a => [a] -> [(a, Int)]
runlength = map (head &&& length) . group

