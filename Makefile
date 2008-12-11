include Config

BUILD := _build
OB := ocamlbuild -no-links -build-dir $(BUILD)
OBCLASSIC := $(OB) -classic-display
ifeq ($(TERM), dumb)
	OB := $(OBCLASSIC)
endif

#################################################################################
BYTE = latex/latex.cma melt/melt.cma melt/tool.byte meltpp/main.byte
NATIVE = latex/latex.cmxa melt/melt.cmxa melt/tool.native
NATIVE11 = $(NATIVE) meltpp/main.native
DOC = latex/latex.docdir/index.html melt/melt.docdir/index.html

default:
	$(OB) $(BYTE) $(DOC)

fast:
	$(OB) $(BYTE)

doc:
	$(OB) $(DOC)

world.10:
	$(OB) $(BYTE) $(DOC) $(NATIVE)

world all:
	$(OB) $(BYTE) $(DOC) $(NATIVE11)
#################################################################################

noob.makefile: clean
	cat noob.prelude > $@
	$(OBCLASSIC) $(BYTE) $(NATIVE) $(DOC) | ob2make default >> $@

install:
	$(OCAML) install.ml -bin $(INSTALLBIN) -lib $(INSTALLLIB) -build $(BUILD)

uninstall:
	$(OCAML) install.ml -bin $(INSTALLBIN) -lib $(INSTALLLIB) -uninstall

clean:
	rm -rf $(BUILD)
	rm -f noob.makefile *~

check bench test %.bench %.check %.test:
	@make -C bench $@

.PHONY: default fast world clean doc all world.10 bench test check

Config: configure.ml
	ocaml configure.ml