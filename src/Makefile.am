bin_PROGRAMS = ../translation

# noinst_PROGRAMS = ../translation

../translation: global.cmo tableDefLexer.cmo tableDef.cmo \
	   	assertionLexer.cmo assertion.cmo \
		insertLexer.cmo insert.cmo \
		deleteLexer.cmo delete.cmo \
		updateLexer.cmo update.cmo \
		main.cmo
	ocamlc -o $@ str.cma global.cmo tableDefLexer.cmo tableDef.cmo \
			assertionLexer.cmo assertion.cmo \
			insertLexer.cmo insert.cmo \
			deleteLexer.cmo delete.cmo \
			updateLexer.cmo update.cmo \
			main.cmo

depend:
	ocamldep *.ml *.mli > .depend

clean:
	rm -rf *.cmi *.cmo $(bin_PROGRAMS)
	rm -rf translation.ml tableDef.mli tableDefLexer.ml tableDefLexer.mli \
			assertion.mli assertionLexer.ml assertionLexer.mli \
			insert.mli insertLexer.ml insertLexer.mli \
			delete.mli deleteLexer.ml deleteLexer.mli \
			update.mli updateLexer.ml updateLexer.mli 

# .SUFFIXES: .ml .mli .mll .mly .cmo .cmi

.ml.cmo:
	ocamlc -c $<
.mli.cmi:
	ocamlc -c $<
.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
.mly.mli:
	ocamlyacc $<

include .depend

ChangeLog::
	hg log --style=changelog --only-branch=default "${top_srcdir}" > $@
