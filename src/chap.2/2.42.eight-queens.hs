--exercise 2.42
--queens :: Int -> [[Int]]
import Debug.Trace
queens board_size = queens_cols board_size
	where queens_cols k
		| k == 0 = empty_board
		| otherwise = filter (\positions -> (is_safe k positions))
						(concatMap (\rest_of_queens -> -- (a->[b]) -> [a] -> [b]
							(map (\new_row -> (adjoin_position new_row k rest_of_queens)) -- [] 
							[1..board_size]))
						(queens_cols (k-1)))

-- (a->[b]) -> [a] -> [b]
-- (rest_of_queens -> [[]]) -> queens_cols(k-1) -> [[]]

-- 집합에 새로운 가로 - 세로 값을 집어넣는 프로시저
adjoin_position row col rest = [row, col]:rest
-- 공집합
empty_board = [[]]

-- 퀸의 위치가 안전한가
-- [[ [], [] .. ] - 첫번째 방법 -, [[]]]
-- [[[1,2],[1,1]],[[2,2],[1,1]],[[1,2],[2,1]],[[2,2],[2,1]]]

-- 현재 1번째 방법을 확인중이라면, 1번째 인덱스의 나머지인덱스에 대해 
-- 같은 diffrow ==0 /diffcol ==0 /diff row == diff col 이면 unsafe
-- k = 2 일때 positions : [ [[1,2], [1,1]] ]
is_safe k (x:[]) = True
is_safe k (x:xs)
	| ((diff_row == 0) || (diff_col == 0) || ((abs diff_row) == (abs diff_col))) = False
 	| otherwise = is_safe k (x:(tail xs))
 	where 
		diff_row = (head x) - (head (head xs))
		diff_col = (head (tail x)) - (head (tail (head xs)))

main = do
	print $ queens 4 --[[[3,4],[1,3],[4,2],[2,1]], [[2,4],[4,3],[1,2],[3,1]]]