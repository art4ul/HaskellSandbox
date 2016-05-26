module Explession where

level:: Int -> String -> Int
level lv (head:tail)  | head == '(' && lv >= 0 = level (lv + 1) tail
                      | head == ')' && lv >= 0 = level (lv - 1) tail
                      | otherwise = lv 
level lv ([]) = lv


{-
 - Checks whether an expression contains a balanced amount of brackets
 - Example:
 -  isBalanced "( ( text2 ) and ( test3 ) )" == True
 -  isBalanced " ( text1 ) text2)" == False
 -} 
isBalanced:: String -> Bool 
isBalanced str = level 0 filteredStr == 0
   where filteredStr = filter (\a -> a =='(' || a == ')') str
