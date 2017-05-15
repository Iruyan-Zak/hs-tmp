main :: IO ()
main = print $ hoge_bind 5

hoge :: Int -> Int
hoge = do
    a <- subtract 2
    (*a)

hoge_bind :: Int -> Int
hoge_bind = do
    subtract 2 >>= (\a ->
        (*a))
hoge' :: Int -> Int
hoge' = (subtract 2) >>= (*)

hoge'' :: Int -> Int
hoge'' = (*) <*> (subtract 2)

hoge''' :: Int -> Int
hoge''' x = x * (x - 2)

