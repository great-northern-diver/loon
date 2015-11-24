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

R:
	./copyTcl2R.sh && \
	cd R && \
	R -e "library(roxygen2); roxygen2::roxygenise()" && \
	cd ../ && \
	R CMD build R --no-build-vignettes && \
	$(RINSTCMD) loon_0.9.1.tar.gz

Roxygen:
	cd R && \
	R -e "library(roxygen2); roxygen2::roxygenise()" && \
	cd ../

website:
	cd website && \
	tclsh makeWebsite.tcl &&  \
	cd ..

debug:
	tclsh8.6 debug.tcl

clean:
