-- 2.32. subsets
-- (), (3), (2), (2, 3), (1), (1,3), (1,2), (1,2,3)

subsets [] = [[]]
subsets (x:xs) = 
	let rest = subsets xs
	in rest ++ (map (x:) rest)

--subsets (x:xs) = 
--	map (:1) xs
-- xs : 2, 3 -> [[1, 2] [1, 3]]
--- want to out list's list... same haskell problem..
main = do
	print $ subsets [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
-- append (car s) rest