# Enable parallel build by default.
BUILDFLAGS=-j 5
LIBDIR=/home/joghurt/ocaml-bin/lib/ocaml
RANLIB=ranlib
OCAMLC=ocamlc
OCAMLOPT=ocamlllvm

# OASIS_START
# DO NOT EDIT (digest: bc1e05bfc8b39b664f29dae8dbd3ebbb)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all: 
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: 
	$(SETUP) -clean $(CLEANFLAGS)

distclean: 
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

asmrun src/asmrun/libasmrun.a:
	cd src/asmrun; make -j 5

asmrun-install: src/asmrun/libasmrun.a
	cd src/asmrun; make install

asmrun-clean:
	cd src/asmrun; make clean


.PHONY: stdlib install-stdlib stdlib-clean
stdlib: src/stdlib/_build/std_exit.o src/stdlib/_build/stdlib.a src/stdlib/_build/stdlib.cmxa
	@touch /dev/null

src/stdlib/_build/std_exit.o src/stdlib/_build/stdlib.a src/stdlib/_build/stdlib.cmxa:
	cd src/stdlib; ocamlbuild
	cd src/stdlib; OCAMLLIB=$(LIBDIR) ocamlbuild $(BUILDFLAGS) -ocamlopt $(OCAMLOPT) stdlib.otarget


stdlib-install: src/stdlib/_build/std_exit.o src/stdlib/_build/stdlib.a src/stdlib/_build/stdlib.cmxa
	cd src/stdlib/_build; cp std_exit.o stdlib.a stdlib.cmxa *.cmi $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) stdlib.a; \
	ln -fs stdlib.cmxa stdlib.p.cmxa; \
        ln -fs stdlib.a stdlib.p.a; \
        ln -fs std_exit.cmx std_exit.p.cmx; \
        ln -fs std_exit.o std_exit.p.o

stdlib-clean:
	cd src/stdlib; ocamlbuild -clean

compile-and-install-everything:
	make
	sudo make install
	make asmrun
	make asmrun-install
	make stdlib-clean
	make stdlib
	make stdlib-install
