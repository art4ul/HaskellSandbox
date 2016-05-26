module PascalTriangle where 

cell :: Int -> Int -> Int
cell x y | x == 1 && y == 1 = 1
         | x == 1 && y /= 1 = 0
         | otherwise = let 
            x1 = x - 1
            y1 = y
            y2 = y - 1
          in cell x1 y1 + cell x1 y2
