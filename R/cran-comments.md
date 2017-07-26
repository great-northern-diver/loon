## Test evironments
* OS X, R 3.4.1
* Ubuntu 17.04, R 3.4.1
* Windows 10, R 3.4.1

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Adrian Waddell <adrian@waddell.ch>’

New submission

## Downstream dependencies

Sometimes R CMD check fails with the ERROR:

Error in MASS::isoMDS(d = D, k = 5) : object 'VR_mds_init_data' not found

This error seems to disappear when I re-install MASS with `install.packages('MASS')`.
