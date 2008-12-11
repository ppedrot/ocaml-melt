default all check bench test:
	ocaml bench.ml *.mlt

%.bench %.check:
	ocaml bench.ml $*.mlt

%.test: %.check
	evince $*.ps

.PHONY: default all check bench test