import Data.List (partition)

qsort :: [a] -> [a]
qsort [] = []
qsort seq = qsort lhs ++ qsort rhs where (lhs, rhs) = partition (< head seq) seq

main = print qsort [3,2,5,2,6,6,14,0,-2,4,7,8,1]
