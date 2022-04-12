module ListPadding where

-- both of these pad the list until it is of length n
lpad :: Int -> a -> [a] -> [a]
lpad n x xs = replicate (n - length xs) x ++ xs

rpad :: Int -> a -> [a] -> [a]
rpad n x xs = xs ++ replicate (n - length xs) x