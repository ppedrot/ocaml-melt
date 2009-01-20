##########################################################################
# Copyright (c) 2009, Romain BARDOU                                      #
# All rights reserved.                                                   #
#                                                                        #
# Redistribution and  use in  source and binary  forms, with  or without #
# modification, are permitted provided that the following conditions are #
# met:                                                                   #
#                                                                        #
# * Redistributions  of  source code  must  retain  the above  copyright #
#   notice, this list of conditions and the following disclaimer.        #
# * Redistributions in  binary form  must reproduce the  above copyright #
#   notice, this list of conditions  and the following disclaimer in the #
#   documentation and/or other materials provided with the distribution. #
# * Neither the  name of Melt nor  the names of its  contributors may be #
#   used  to endorse  or  promote products  derived  from this  software #
#   without specific prior written permission.                           #
#                                                                        #
# THIS SOFTWARE  IS PROVIDED BY  THE COPYRIGHT HOLDERS  AND CONTRIBUTORS #
# "AS  IS" AND  ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT #
# LIMITED TO, THE IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR #
# A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT  SHALL THE COPYRIGHT #
# OWNER OR CONTRIBUTORS BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, #
# SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT #
# LIMITED TO, PROCUREMENT OF SUBSTITUTE  GOODS OR SERVICES; LOSS OF USE, #
# DATA, OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED AND ON ANY #
# THEORY OF  LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY,  OR TORT #
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY OUT OF THE USE #
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   #
##########################################################################

include Config

BUILD := _build
OB := ocamlbuild -no-links -build-dir $(BUILD) -Is latex,meltpp,melt
OBCLASSIC := $(OB) -classic-display
ifeq ($(TERM), dumb)
	OB := $(OBCLASSIC)
endif

#################################################################################
BYTE = latex/latex.cma melt/melt.cma melt/tool.byte meltpp/main.byte
NATIVE = latex/latex.cmxa melt/melt.cmxa melt/tool.native
NATIVE11 = $(NATIVE) meltpp/main.native
DOC = latex/latex.docdir/index.html melt/melt.docdir/index.html
BENCHPLUGS = bench/plugs/quot.cma

default: world.10

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
	rm -rf $(BUILD) bench/_melt
	rm -f noob.makefile *~

check bench test %.bench %.check %.test:
	$(OB) $(BYTE) $(BENCHPLUGS)
	@make -C bench $@

dist: $(shell darcs query manifest) noob.makefile
	tar czf melt.tgz $^

.PHONY: default fast world clean doc all world.10 bench test check dist

Config: configure.ml
	ocaml configure.ml