{-# LANGUAGE MultiParamTypeClasses, OverlappingInstances, FlexibleInstances #-}

-- cf. https://www.haskell.org/tutorial/classes.html

-- haskell class : 
-- Type classes conveniently solve both of these problems. 
-- They allow us to declare which types are instances of which class, 
-- and to provide definitions of the overloaded operations associated with a class.
-- it is CONTEXT
class Fringe a where
  -- recv list -> out list
  -- how it can be possible that override even if its' relations? 
  -- sometimes ([1,2]) I want to fringe :: [a] -> [a]
  -- and sometimes ([[1],[2]]) I want to fringe :: [a] -> a
  fringe :: [a] -> a

instance Fringe a => Fringe [a] where 
  --fringe x = x  -- / [1,2] -1 번째 인덱스 / [3,4] - 2 번째 인덱스 
  fringe x = do b <- x
                c <- b
                return c

instance Fringe a where
  fringe = undefined

main = do
  --print $ (fringe [1,2,3,4] :: [Int])
  print $ (fringe [[[1,2]],[[3,4]]] :: [[Int]]) --[[1,2],[3,4]]
  print $ (fringe [[1,2], [3,4]] :: [Int]) --[1,2,3,4]

-- exercise 2.28. Fringe
--import Debug.Trace
--fringe (x:[]) = [x]
--fringe (x:xs) = trace ("x:"++show x++",xs"++show xs) (fringe [x] ++ fringe xs )

---- scheme 에서는 pair 로 단일인지 쌍인지 같은 함수에서 구분했던거보면 
---- 그냥 이렇게 풀면 되는거 같다... 

--main = do
--  print $ fringe [[1,2], [3,4]]
--  --print $ fringe [1,2,3,4]
--  --print $ test [1,2,3,4]