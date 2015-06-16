--2.43. improve_queens
import Debug.Trace

queens board_size = queens_cols board_size
	where queens_cols k
		| k == 0 = empty_board
		| otherwise = filter (\positions -> (is_safe k positions))
						(concatMap (\rest_of_queens -> 
							(map (\new_row -> trace (show 1) (adjoin_position new_row k rest_of_queens)) -- [] 
							[1..board_size]))
						(queens_cols (k-1)))

queens1 board_size = queens_cols board_size
	where queens_cols k
		| k == 0 = empty_board
		| otherwise = filter (\positions -> (is_safe k positions))
						(concatMap (\new_row -> 
							(map (\rest_of_queens -> trace (show 1) (adjoin_position new_row k rest_of_queens)) -- [] 
							(queens_cols (k-1))))
						[1..board_size])

-- 집합에 새로운 가로 - 세로 값을 집어넣는 프로시저
adjoin_position row col rest = [row, col]:rest
-- 공집합
empty_board = [[]]

-- 퀸의 위치가 안전한가
is_safe k (x:[]) = True
is_safe k (x:xs)
	| ((diff_row == 0) || (diff_col == 0) || ((abs diff_row) == (abs diff_col))) = False
 	| otherwise = is_safe k (x:(tail xs))
 	where 
		diff_row = (head x) - (head (head xs))
		diff_col = (head (tail x)) - (head (tail (head xs)))



main = do
	-- 60
	print $ queens1 4 -- 624