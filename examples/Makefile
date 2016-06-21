example:
	ocamlc -ppx ppx_debugger -dsource -g sample.ml
	@rm -rf *.cm*
	@echo "RUN PROGRAM"
	@echo "==========="
	@./a.out

example-nt:
	ocamlc sample.ml

show:
	ocamlfind ppx_tools/dumpast sample.ml

clean:
	rm -rf *.cm*
