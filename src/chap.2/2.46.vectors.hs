--2.46.vectors 

make_vect x y = (x,y)
xcor_vect = fst
ycor_vect = snd
add_vect v1 v2 = make_vect ((xcor_vect v1) + (xcor_vect v2))
                           ((ycor_vect v1) + (ycor_vect v2))

sub_vect v1 v2 = make_vect ((xcor_vect v1) - (xcor_vect v2))
                           ((ycor_vect v1) - (ycor_vect v2))

scale_vect s v = make_vect (s*(xcor_vect v)) (s*(ycor_vect v))

main = do
  print $ make_vect 1 2 -- (1,2)
  print $ xcor_vect $ make_vect 1 2  -- 1
  print $ ycor_vect $ make_vect 1 2  -- 2
  print $ add_vect (make_vect 1 2) (make_vect 3 4) -- (4, 6)
  print $ sub_vect (make_vect 3 4) (make_vect 1 2) -- (2, 2)
  print $ scale_vect 2 (make_vect 1 2) -- (2,4)