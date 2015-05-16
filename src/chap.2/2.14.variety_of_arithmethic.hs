-- 대수적 표현의 차이 체계를 조사해보라. 
-- A 와 B 의 구간을 만들고, A/A 와 A/B 를 계산해보라
-- 구간의 중심으로부터 percentage 가 작을때 이런 관찰이 더 간편할 것이다.
-- center - percent form 으로 결과를 실험해보라. 

make_center_width c w =
  make_interval (- c w) (+ c w)
--center i = div ((lower_bound i) + (upper_bound i)) 2 
--width i = div ((upper_bound i) - (lower_bound i)) 2

make_center_percent c p = 
  make_interval (c - (c * p * 0.01)) (c + (c * p * 0.01))

cons x y = (\m-> m x y)
car z = z (\p q-> p)
cdr z = z (\p q-> q)

make_interval x y = cons x y

-- 인자로 받은 함수에 두개의 인자를 받는 함수를 넘긴다.
lower_bound iv = iv (\p q-> min p q)
upper_bound iv = iv (\p q-> max p q)

div_interval iv1 iv2
  | (upper_bound iv2 == 0 || lower_bound iv2 == 0) = undefined
  | otherwise = mul_interval iv1 (make_interval (1 / (upper_bound iv2)) (1 / (lower_bound iv2)))

mul_interval iv1 iv2 = let 
  p1 = (lower_bound iv1) * (lower_bound iv2)
  p2 = (lower_bound iv1) * (upper_bound iv2)
  p3 = (upper_bound iv1) * (lower_bound iv2) 
  p4 = (upper_bound iv1) * (upper_bound iv2) 
  in make_interval (minimum [p1, p2, p3, p4]) (maximum [p1, p2, p3, p4])

display iv = 
  print (lower_bound iv, upper_bound iv)

a = (make_center_percent 30 0.0000023)
b = (make_center_percent 30 0.00000004)
main = do 
  display a
  display b
  display $ div_interval a a
  display $ div_interval a b
  --print $ (3 - 3*10)

--(29.99999931,30.00000069)
--(29.999999988,30.000000012)
--(0.9999999540000011,1.0000000460000011)
--(0.9999999766,1.0000000234)
--[Finished in 0.4s]