module MergeSort where 

sort :: (Ord a) => [a] -> [a] 
sort input@(head : []) = input
sort input@(head:tail) = merge leftSorted rightSorted []
   where (left , right)     = splitAtTheMiddle input
         leftSorted         = sort left
         rightSorted        = sort right
         splitAtTheMiddle x = let
            middle = (length x) `div` 2         
            in splitAt middle x


merge :: (Ord a) => [a] -> [a] -> [a] ->[a]
merge [] right result =  result ++ right
merge left [] result  =  result ++ left
merge left@(lhead:ltail) right@(rhead:rtail) prev 
   | lhead < rhead    =  merge ltail right $ listWith lhead
   | otherwise        =  merge left rtail  $ listWith rhead
   where listWith x = prev ++ [x]
