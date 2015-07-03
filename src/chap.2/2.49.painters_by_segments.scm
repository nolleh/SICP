(define (segments->painter segment-list)
	(lambda (frame)
		(for-each
			(lambda (segment)
				(draw-line
					((frame-coord-map frame) (start-segment segment))
					((frame-coord-map frame) (end-segment segment))))
	segment-list)))

; define segment as : 2 list of vector [(x,y)]
; (0 <= x, y <= 0)
; a. 그림틀의 테두리를 그려주는 페인터
(segments->painter [ [(0,0),(0,1)], [(0,1),(1,1)], [(1,1),(1,0)], [(1,0),(0,0)] ])
; b. 그림틀에서 마주보는 꼭짓점을 연결, 'X' 를 그리는 페인터
(segemets->painter [ [(0,1), (1,0)], [(1,1), (0,0)] ])
; c. 그림틀의 모서리 가운데 네개를 연결, 다이아몬드 꼴을 그리는 페인터
(segments->painter [ [(0,0.5), (0.5,1)], [(0.5,1), (1,0.5)], [(1,0.5), (0.5,0)], [(0.5,0), (0,0.5)] ])
; d. wave 페인터
; ()