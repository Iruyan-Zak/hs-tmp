import Data.List (sort)
import Data.Char (toUpper)

main = words "world! hello," |> sort |> unwords |> map toUpper |> putStrLn

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

