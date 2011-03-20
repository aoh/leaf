
((fn (y)
	((fn (pair t f)
		((fn (nil nil? rule)
			((fn (walk)

(def (see l more)
	(nil? l
		more
		(cons
			(l t 111 32)
			(see (l f) more))))

(def (evolve level)
	(cons 60
		(see level
			(cons 62
				(cons 10 
					(evolve
						(walk f (level t) (level f))))))))

(evolve 
	(pair f (pair t (pair f nil)))))

(y (fn (walk a b l)
	(nil? l
		(pair (rule a b f) (pair f nil))
		(pair (rule a b (l t))
			(walk b (l t) (l f))))))))
(fn (c a b) t)
(fn (l)  (l t f f))
(fn (a b c) (a (b (c f t) (c t t)) (b (c t t) (c f f))))))
(fn (a b c) (c a b))
(fn (a b) a)
(fn (a b) b)))
(fn (a) ((fn (x) (x x)) (fn (b) (a (b b))))))


