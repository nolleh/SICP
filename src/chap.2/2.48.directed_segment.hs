make_vect x y = (x,y)
xcor_vect = fst
ycor_vect = snd

-- 원점 > 시작점 (벡터)
-- 원점 > 끝점 (벡터)
make_segment sv ev = make_vect sv ev
start_segment = xcor_vect
end_segment = ycor_vect

main = do
	print $ start_segment $ make_segment (1,2) (3,4)
	print $ end_segment $ make_segment (1,2) (3,4)