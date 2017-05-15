module Safe where

import Prelude hiding (init, last)

head :: [a] -> Maybe a
head xs = do
    x:_ <- Just xs
    return x

tail :: [a] -> Maybe [a]
tail xs = do
    _:xs' <- Just xs
    return xs'

init :: [a] -> Maybe [a]
init [] = Nothing
init [_] = Just []
init (x:xs) = (x:) <$> init xs

last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (_:xs) = last xs

