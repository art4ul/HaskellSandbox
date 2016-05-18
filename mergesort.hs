module MergeSort where 

   merge :: (Ord a) => [a] -> [a] -> [a] ->[a]
   merge [] right result =  result ++ right
   merge left [] result  =  result ++ left
   merge left@(lhead:ltail) right@(rhead:rtail) prev 
      | lhead < rhead = let result = prev ++ [lhead] in merge ltail right result
      | otherwise = let result = prev ++ [rhead] in merge left rtail result


   sort :: (Ord a) => [a] -> [a] 
   sort input@(head : []) = input
   sort input@(head:tail) = 
        let 
           (left , right) = splitAtTheMiddle input 
           leftSorted = sort left 
           rightSorted = sort right
        in 
           merge leftSorted rightSorted []


   splitAtTheMiddle :: (Ord a) => [a] -> ([a],[a])
   splitAtTheMiddle x = 
      let
         middle = (length x) `div` 2
      in
        splitAt middle x 
