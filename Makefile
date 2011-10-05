# test-analyzer:
# 	raco make -v --disable-inline test-analyzer.rkt
# 	racket test-analyzer.rkt
all: planet-link launcher setup


bump-version:
	racket bump-version.rkt

launcher: 
	racket make-launcher.rkt

test-all: 
	racket tests/test-all.rkt

test-browser-evaluate: 
	racket tests/test-browser-evaluate.rkt

test-compiler: 
	racket tests/test-compiler.rkt


test-parse-bytecode-on-collects: 
	racket tests/test-parse-bytecode-on-collects.rkt


test-earley: 
	racket tests/test-earley.rkt


test-conform: 
	racket tests/test-conform.rkt

test-more: 
	racket tests/run-more-tests.rkt

doc:
	scribble  ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --dest generated-docs  --dest-name index.html scribblings/manual.scrbl


cs019-doc:
	scribble  ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --dest generated-docs  scribblings/cs019.scrbl



setup:
	raco setup --no-docs -P dyoo whalesong.plt 1 5


planet-link:
	raco planet link dyoo whalesong.plt 1 5 .