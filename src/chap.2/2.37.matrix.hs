--exercise 2.37
accumulate_n op initial [] = []
accumulate_n op initial seqs@(x:xs) =
  case x of
      [] -> [] 
      otherwise -> (foldr op initial (map head seqs)) : (accumulate_n op initial (map tail seqs))

map' proc v w = iter proc v w []
  where iter proc v w out = 
          case v of 
            [] -> out
            otherwise -> iter proc (tail v) (tail w) (out++[(proc (head v) (head w))]) 

      --otherwise -> 1:[1]
-- vector dot_product
--[1,2] x [10,20] = 1*10 + 2*20 = 50
dot_product v w = foldr (+) 0 (map' (*) v w)

-- matrix * vector m v (t i = Ej Mij vj)
-- [1,2] * [5,6] = [1*5 + 2*6] = [17]
-- [3,4]           [3*5 + 4*6]   [39]
mat_vec_product m v = map (dot_product v) m

-- transepose m = nij = mji
-- [1,2] = [1,3]
-- [3,4] = [2,4]
transepose m = accumulate_n (:) [] m

--matrix dot_product 
--[1,2] X [5,6] = [5+14 , 6+16]
--[3,4]   [7,8]   [15+28, 18+32]
mat_mat_product m n = 
  let cols = transepose n
  --in map' dot_product m cols -- [19,50]
  --in map (map dot_product m) cols
  in list_func (map dot_product m) cols
  where list_func funcs@[] _ = []
        list_func funcs@(x:xs) args@[] = list_func xs (transepose n)
        list_func funcs@(x:xs) args@(y:ys) = (x y) : list_func funcs ys


main = do
  print $ dot_product [1,2] [10,20] -- 50
  print $ mat_vec_product [[1,2], [3,4]] [5,6] -- [17,39]
  print $ transepose [[1,2],[3,4]] -- [[1,3],[2,4]]
  print $ mat_mat_product [[1,2],[3,4]] [[5,6],[7,8]] -- [19,22,43,50]