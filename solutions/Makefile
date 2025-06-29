# Makefile - Approximations numériques
SRC := Grimaud_Numerical_Approximations
EXE := approx

.PHONY: clean

$(EXE): $(SRC).ml
	ocamlopt -o $@ $<

clean:
	rm *.cmi *.cmo *.cmx $(SRC).o $(EXE)
