#!/bin/sh

if [ -d "Python/diver/Tcl/images" ]; then
    echo "Delete Python Tcl images folder"
    rm -R Python/diver/Tcl/images
fi

if [ -d "Python/diver/Tcl/library" ]; then
    echo "Delete Python Tcl library folder"
    rm -R Python/diver/Tcl/library
fi

#mkdir -p Python/diver/Tcl

cp Tcl/pkgIndex.tcl Python/diver/Tcl/
rsync -av --delete Tcl/library Python/diver/Tcl/
rsync -av --delete Tcl/images Python/diver/Tcl/

