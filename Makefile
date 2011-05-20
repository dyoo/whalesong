test-all:
	raco make -v --disable-inline tests/test-all.rkt
	racket tests/test-all.rkt


test-compiler:
	raco make -v --disable-inline tests/test-compiler.rkt
	racket tests/test-compiler.rkt


test-earley:
	raco make -v --disable-inline tests/test-earley.rkt
	racket tests/test-earley.rkt


test-conform:
	raco make -v --disable-inline tests/test-conform.rkt
	racket tests/test-conform.rkt

