-- list
-- head => origin / tail => edge1 : edge2
origin_frame :: [(Int, Int)] -> (Int, Int)
origin_frame frame = head frame

edge1_frame :: [(Int, Int)] -> (Int, Int)
edge1_frame frame = (head (tail frame))

edge2_frame :: [(Int, Int)] -> (Int, Int)
edge2_frame frame = (head (tail (tail frame)))

-- list 
-- in scheme.. 
--(define (origin_frame frame)
--  car frame)

--(define (edge1_frame frame)
--  cadr frame) -- (car (cdr frame))

--(define (edge2_frame frame)
--  (caddr frame))) -- (car (cdr (cdr frame))))


-- cons
-- in scheme .. 
--(define (origin_frame frame))
--  car frame)

--(define (edge1_frame frame)
--  cadr frame))

--(define (edge2_frame frame)
--  (cddr frame))  -- (cdr (cdr frame))

main = do
  print $ origin_frame [(1,1), (2,2), (3,3)]
  print $ edge1_frame [(1,1), (2,2), (3,3)]
  print $ edge2_frame [(1,1), (2,2), (3,3)]