#!/bin/sh

if [ -d "R/inst/tcl" ]; then
    echo "Delete inst/tcl folder"
    rm -R R/inst/tcl
fi

mkdir -p R/inst/tcl/loon

cp Tcl/pkgIndex.tcl R/inst/tcl/loon
rsync -av --delete Tcl/library R/inst/tcl/loon
rsync -av --delete Tcl/images R/inst/tcl/loon

# rsync -av --delete website/html R/inst/website
# rm R/inst/website/html/.gitignore
