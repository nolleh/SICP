-- exercise 2.8
-- define sub-interval
cons x y = (\m-> m x y)
car z = z (\p q-> p)
cdr z = z (\p q-> q)

make_interval x y = cons x y

-- 인자로 받은 함수에 두개의 인자를 받는 함수를 넘긴다.
lower_bound iv = iv (\p q-> min p q)
upper_bound iv = iv (\p q-> max p q)

sub_interval iv1 iv2 = 
	make_interval ((lower_bound iv1) - (lower_bound iv2)) ((upper_bound iv1) - (upper_bound iv2)) 

display iv = do
	print $ lower_bound iv 
	print $ upper_bound iv

main = display (sub_interval (make_interval 3 5) (make_interval 1 5))