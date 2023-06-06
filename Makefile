
MODNAMES=BDDType HashConsingHashtbl BDD
MODS=$(addprefix robdd., $(MODNAMES))

all: robdd.mlw
	why3 -L . extract -D ocaml64 -D robdd-driver.drv --modular -o extraction $(MODS)
	cd extraction && dune build
