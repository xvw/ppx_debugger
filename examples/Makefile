example:
	ocamlc -ppx ../ppx_debugger.native -dsource  sample.ml
	@rm -rf *.cm*
	@echo "RUN PROGRAM"
	@echo "==========="
	@./a.out

show:
	ocamlfind ppx_tools/dumpast sample.ml
