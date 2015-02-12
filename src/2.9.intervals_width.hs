-- exercise 2.9 
-- width : upper_bound - lower_bound / 2
-- 두 구간의 합/차 로 얻은 구간의 width 가 피연산 구간들의 width의 합/차임을 보이라. 

-- 1. new_width = (upper_bound new_interval) - (lower_bound new_interval)
-- 2. .. = (upper_bound (make_interval (lower_bound iv1 + lower_bound iv2) 
--								    (upper_bound iv1 + upper_bound iv2) ) ) -
--         (lower_bound (make_interval (....) ) )
-- 3.    = (upper_bound iv1 + upper_bound iv2) - (lower_bound iv1 + lower_bound iv2)
-- 4.    = (upper_bound iv1 - lower_bound iv1) + (upper_bound iv2 - lower_bound iv2)
-- 따라서, 피연산구간들의 width 의 합 = 두 구간의 합의 width 이다. 

cons x y = (\m-> m x y)
car z = z (\p q-> p)
cdr z = z (\p q-> q)

make_interval x y = cons x y

-- 인자로 받은 함수에 두개의 인자를 받는 함수를 넘긴다.
lower_bound iv = iv (\p q-> min p q)
upper_bound iv = iv (\p q-> max p q)

mul_interval iv1 iv2 = let 
	p1 = (lower_bound iv1) * (lower_bound iv2)
	p2 = (lower_bound iv1) * (upper_bound iv2)
	p3 = (upper_bound iv1) * (lower_bound iv2) 
	p4 = (upper_bound iv1) * (upper_bound iv2) 
	in make_interval (minimum [p1, p2, p3, p4]) (maximum [p1, p2, p3, p4])
-- 피연산 구간들의 width 의 곱이 두 구간의 곱의 width 와 상이하려면
-- 피연산 구간들의 width 곱 = 
-- 		(upper_bound iv1 - lower_bound iv1) * (upper_bound iv2 - lower_bound iv2) 
--  upper_iv1 * upper_iv2 - upper_iv1 * lower_iv2 
--     - lower_iv1 * upper_iv2 + lower_iv1 * lower_iv2 

display iv = do
	print $ lower_bound iv 
	print $ upper_bound iv

main = let 
		iv1 = make_interval 3 5
		iv2 = make_interval 4 5
		in do 
			print $ (upper_bound iv1 - lower_bound iv1) * (upper_bound iv2 - lower_bound iv2)
			print $ "---- 3*4 / 5*5 ----"
			display (mul_interval iv1 iv2)