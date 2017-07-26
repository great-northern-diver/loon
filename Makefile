# Linux needs Administrator Passwords to Install R packages
ifeq ($(OS),Windows_NT)
	RINSTCMD := R CMD INSTALL
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Linux)
		RINSTCMD := R CMD INSTALL
	endif
	ifeq ($(UNAME_S),Darwin)
		RINSTCMD := R CMD INSTALL
	endif
endif

# http://stackoverflow.com/questions/1789594/how-to-write-cd-command-in-makefile



## Make sure that make does not think
## that the rules are associated to an actual file
.PHONY: all Tcl R website clean debug

all: Tcl R website

Tcl:
	echo "Make Tcl Package\n-----------"  && \
	cd Tcl && ./makePkgIndex.tcl && cd ..

R: copyTcl2R rdoc rbuild rinst

copyTcl2R:
	./copyTcl2R.sh

rdoc:
	cd R && \
	R -e "library(devtools); devtools::document(roclets=c('rd', 'namespace'))" && \
	cd ../

rbuild:
	R CMD build R --no-build-vignettes

rinst:
	$(RINSTCMD) loon_1.1.0.tar.gz

website:
	cd website && \
	tclsh makeWebsite.tcl &&  \
	cd .. && \
	rsync -av --delete website/html/ ./docs/ && \
	rm ./docs/.gitignore

debug:
	tclsh8.6 debug.tcl

clean:
