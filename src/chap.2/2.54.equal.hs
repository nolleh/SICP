-- (equal? '(this is a list) '(this is a list))

-- (equal? '(this is a list) '(this (is a) list))
-- all is true => true
equals [] [] = True
equals (x:_) [] = False
equals [] (x:_) = False -- 개수 체크 ... 안 이쁘지만 뭐...
equals list1@(x:xs) list2@(y:ys)
	| x == y = equals xs ys -- (eq? a b) and.. (eq? (car a) (car b)) and (eq? (cdr a)..)
	| otherwise = False

main = do 
	print $ equals ["this", "is", "a", "list"] ["this", "is", "a", "list"] -- True
	print $ equals ["this", "is", "a", "list"] ["this", "is a", "list"] -- False
	-- Couldn't match expected type ‘Char’ with actual type ‘[Char]’
	--print $ equals ["this", "is", "a", "list"] ["this", ["is"], "a", "list"]