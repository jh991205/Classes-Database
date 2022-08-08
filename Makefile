.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src


test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

update_current:
	OCAMLRUNPARAM=b dune exec update/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

zip:
	rm -f project.zip
	zip -r project.zip . -x@exclude.lst

clean: bisect-clean
	dune clean
	rm -f project.zip

doc:
	dune build @doc