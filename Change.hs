module Change where 
import Data.List

countChange :: Int -> [Int] -> Int
countChange 0 _ = 1
countChange money _ | money < 0  = 0
countChange money [] = 0 
countChange money coins@(head:tail) = countChange money tail  + countChange (money - head) coins 
