#!/bin/sh

if [ -d "R/inst"]; then
    echo "Delete inst folder"
    rm -R R/inst
fi

mkdir -p R/inst/tcl/loon

cp Tcl/pkgIndex.tcl R/inst/tcl/loon
rsync -av --delete Tcl/library R/inst/tcl/loon
rsync -av --delete Tcl/images R/inst/tcl/loon

# rsync -av --delete website/html R/inst/website
# rm R/inst/website/html/.gitignore
