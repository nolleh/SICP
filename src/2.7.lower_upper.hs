--exercise 2.7
-- define upper-bound / lower-bound

cons x y = (\m-> m x y)
car z = z (\p q-> p)
cdr z = z (\p q-> q)

make_interval x y = cons x y

-- 인자로 받은 함수에 두개의 인자를 받는 함수를 넘긴다.
lower_bound iv = iv (\p q-> min p q)
upper_bound iv = iv (\p q-> max p q)

main = do
	print $ lower_bound (make_interval 2 4)
	print $ upper_bound (make_interval 3 5)