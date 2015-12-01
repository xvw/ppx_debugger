OCAMLC = ocamlc -ppx ../ppx_debugger.native

ifeq ($(DSOURCE), yes)
	SOURCE = -dsource
endif


all: sample1
	./sample1.byte
	rm -rf *.byte
	rm -rf *.cm*
	rm -rf *~


sample1:
	$(OCAMLC) $(SOURCE) sample1.ml -o sample1.byte

clean:
	rm -rf *.byte
	rm -rf *.cm*
	rm -rf *~
