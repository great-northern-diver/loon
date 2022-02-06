# call make file in shell to rebuild tcl AND the tcl/R website
# Source tcl is in Tcl folder above R;
# make replaces the tcl library inside R
#
# build documentation
# check as CRAN
pkgdown::build_site()
devtools::check_win_devel()
