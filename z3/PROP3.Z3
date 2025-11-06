
(declare-const p bool)
(declare-const q bool)
(declare-const r bool)
(define-fun conjecture () bool
	(=> (and (=> p q) (=> q r))
		(=> p r)))
(assert (not conjecture))
(check-sat)

