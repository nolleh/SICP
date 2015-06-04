--exercise 2.39
--reverse_r = foldr (\x y -> y:x) [] --  []:1 , cf.cons 는 1:[] 와 같은 형태만 동작
reverse_r = foldr (\x y -> y++[x]) []
reverse_l = foldl (\x y -> y:x) []

main = do
	print $ reverse_r [1,2,3,4] -- [4,3,2,1]
	print $ reverse_l [1,2,3,4] -- [4,3,2,1]