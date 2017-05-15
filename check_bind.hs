main :: IO ()
main = print $ Just 1 >> (Nothing :: Maybe Integer) >>= hoge >> Just 3

hoge :: Maybe Integer -> Maybe Integer
hoge Nothing = Just 0
hoge _ = Nothing

