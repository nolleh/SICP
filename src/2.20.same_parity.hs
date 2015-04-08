-- 2.20. same_parity
-- variable parameter.
import Debug.Trace
-- 하스켈에서는 그냥 리스트로 받으면된다
same_parity [] = error "empty"
same_parity (x:xs) = iter remainder xs [x]
  where
    remainder = mod x 2 
    iter remainder items out
      | null items = out
      | (mod (head items) 2) == remainder = 
          trace (show items++show out) 
          (iter remainder (tail items) (out++[(head items)]))
      | otherwise = (iter remainder (tail items) out)

main = do
  print $ same_parity [1, 2, 3, 4, 5, 6, 7]
  print $ same_parity [2, 3, 4, 5, 6, 7]

--[3,4,5,6,7][1]
--[5,6,7][1,3]
--[7][1,3,5]
--[4,5,6,7][2]
--[6,7][2,4]

--[1,3,5,7]
--[2,4,6]