test-compiler:
	raco make -v --disable-inline test-compiler.rkt
	racket test-compiler.rkt


test-earley:
	raco make -v --disable-inline test-earley.rkt
	racket test-earley.rkt


test-conform:
	raco make -v --disable-inline test-conform.rkt
	racket test-conform.rkt
