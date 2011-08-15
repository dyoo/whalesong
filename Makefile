# test-analyzer:
# 	raco make -v --disable-inline test-analyzer.rkt
# 	racket test-analyzer.rkt

launcher: last-commit-name
	raco make -v --disable-inline whalesong.rkt
	racket make-launcher.rkt

whalesong: last-commit-name
	raco make -v --disable-inline whalesong.rkt

test-all: last-commit-name
	raco make -v --disable-inline tests/test-all.rkt
	racket tests/test-all.rkt

test-browser-evaluate: last-commit-name
	raco make -v --disable-inline tests/test-browser-evaluate.rkt
	racket tests/test-browser-evaluate.rkt

test-compiler: last-commit-name
	raco make -v --disable-inline tests/test-compiler.rkt
	racket tests/test-compiler.rkt


test-parse-bytecode-on-collects: last-commit-name
	raco make -v --disable-inline tests/test-parse-bytecode-on-collects.rkt
	racket tests/test-parse-bytecode-on-collects.rkt


test-earley: last-commit-name
	raco make -v --disable-inline tests/test-earley.rkt
	racket tests/test-earley.rkt


test-conform: last-commit-name
	raco make -v --disable-inline tests/test-conform.rkt
	racket tests/test-conform.rkt

test-more: last-commit-name
	raco make -v --disable-inline tests/run-more-tests.rkt
	racket tests/run-more-tests.rkt



last-commit-name:
	racket make-last-commit-name.rkt

doc: last-commit-name
	scribble  ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --dest generated-docs  --dest-name index.html scribblings/manual.scrbl


setup: last-commit-name
	raco setup -P dyoo whalesong.plt 1 2
