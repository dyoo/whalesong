# test-analyzer:
# 	raco make -v --disable-inline test-analyzer.rkt
# 	racket test-analyzer.rkt

launcher: 
	raco make -v --disable-inline whalesong.rkt
	racket make-launcher.rkt

whalesong:
	raco make -v --disable-inline whalesong.rkt

test-all:
	raco make -v --disable-inline tests/test-all.rkt
	racket tests/test-all.rkt

test-browser-evaluate:
	raco make -v --disable-inline tests/test-browser-evaluate.rkt
	racket tests/test-browser-evaluate.rkt

test-compiler:
	raco make -v --disable-inline tests/test-compiler.rkt
	racket tests/test-compiler.rkt


test-parse-bytecode-on-collects:
	raco make -v --disable-inline tests/test-parse-bytecode-on-collects.rkt
	racket tests/test-parse-bytecode-on-collects.rkt


test-earley:
	raco make -v --disable-inline tests/test-earley.rkt
	racket tests/test-earley.rkt


test-conform:
	raco make -v --disable-inline tests/test-conform.rkt
	racket tests/test-conform.rkt




doc:
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --dest generated-docs --dest-name index.html scribblings/manual.scrbl