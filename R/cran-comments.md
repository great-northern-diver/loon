## Test evironments

* local OS X, R 3.5.1
* local Ubuntu 18.04 R 3.5.1 and R-devel (2018-09-29 r75381)
* local Windows 10 R 3.5.1
* Ubuntu 14.04.5 (on travis-ci) R 3.5.1 and R-devel (2018-09-29 r75381)

## R CMD check results

There were no ERRORs or WARNINGs.

## Downstream dependencies

Sometimes R CMD check fails with the ERROR:

Error in MASS::isoMDS(d = D, k = 5) : object 'VR_mds_init_data' not found

This error seems to disappear when I re-install MASS with `install.packages('MASS')`.
