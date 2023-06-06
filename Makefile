MODNAMES=BDDType HashConsingHashtbl BDDAssociationMap BDDTupleAssociationMap BDD Not Size And Or Implies
MODS=$(addprefix robdd., $(MODNAMES))

all: robdd.mlw
	why3 -L . extract -D ocaml64 -D robdd-driver.drv --modular -o extraction $(MODS)
	cd extraction && dune build

queens: all
	cd extraction && dune exec ./queens.exe

smoke:
	why3 replay --smoke-detector=top robdd-no-smoke

replay:
	why3 replay robdd
