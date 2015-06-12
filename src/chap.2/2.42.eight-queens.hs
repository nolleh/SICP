--exercise 2.42
queens board_size = queens_cols board_size
	where queens_cols k
		| k == 0 = empty_board
		| otherwise = filter (\positions -> (is_safe k positions))
						concatMap (\rest_of_queens -> 
							map (\new_row -> adjoin_position new_row k rest_of_queens)) [1..board_size]
						queens_cols (k-1)

-- 집합에 새로운 가로 - 세로 값을 집어넣는 프로시저
adjoin_position = 0
-- 공집합
empty_board = []
-- 퀸의 위치가 안전한가
is_safe = True

main = do
	print $ queens 8