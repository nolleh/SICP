--deriv g = 

-- int -> int 인 함수를 받아서 다음에 int를 받으면 
-- float을 리턴하는 함수를 리턴
deriv g = (\x -> (g (x+dx) - g x) / dx)
	where dx = 0.00001

cube x = x * x * x
square x = x * x
-- f(x) = x - g(x)/Dg(x)

-- 뉴튼 변환(f x)은 (g x) = 0 일때 정점(fixed point)이다. 
-- 다시 말해서 (f x) 가 정점이면 g(x) 도 0이다 ? 
-- (g x) 값이 0 이 되는 x 값을 구하고 싶음.
--  
newtons_transform g = 
	(\x -> x - (g x) / deriv (g x))
newtons_method g guess =
	fixed_point (newtons_transform g) guess

main = do
	print $ ((deriv cube) 5)
	print $ (newtons_method (\y -> square y - x) 4)