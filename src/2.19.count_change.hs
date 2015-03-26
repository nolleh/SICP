-- exercise 2.19
-- change counting

cc amount coin_values
  | amount == 0 = 1
  | amount < 0 && (no_more coin_values) = 0
  | otherwise = (cc amount (except_first_denomination coin_values)) +
                (cc (amount - (first_denomination coin_values)) coin_values)

--- 왜 컴파일 안됨 ??

first_denomination [] = 0
first_denomination xs = head xs
except_first_denomination [] = 0
except_first_denomination xs = head $ tail xs
no_more xs 
  | null xs = True
  | otherwise = False

main = do
  print $ cc 100 [10, 20]