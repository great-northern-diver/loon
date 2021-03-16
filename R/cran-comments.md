## Test evironments

* local OSX, Big Sur 11.2.1,  R 4.0.2
* Ubuntu Xenial 16.04  (on travis-ci)
* check_win_devel 
  - using R Under development (unstable) (2021-03-14 r80087)
  - using platform: x86_64-w64-mingw32 (64-bit)
  - using session charset: ISO8859-1
  
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

## R CMD check results

0 errors
0 warnings
1 NOTE about maintainer

  ... NOTE
  Maintainer: 'R. Wayne Oldford <rwoldford@uwaterloo.ca>'

### Solaris and bioconductor package RgraphViz

**rhub::check_on_solaris()**:

1 Error (cannot find/install suggested package `Rgraphviz)`

  - Unable to determine a solution.  (have tried adding BiocViews: to DESCRIPTION with no luck)

- Preparing build, see status at

  https://builder.r-hub.io/status/loon_1.3.4.tar.gz-a8636ac65fcf46899d204dc1c39bafce

...

  E  checking package dependencies (3.5s)
     Package suggested but not available: ‘Rgraphviz’
   
     The suggested packages are required for a complete check.
     Checking can be attempted without them by setting the environment
     variable _R_CHECK_FORCE_SUGGESTS_ to a false value.
   
     See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
     manual.
   

**Relevant Snippet from Rhub**:

- Installing package dependencies\

- Running R CMD check

  setting _R_CHECK_FORCE_SUGGESTS_ to false
   
  setting R_COMPILE_AND_INSTALL_PACKAGES to never
   
  setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
   
  setting R_REMOTES_STANDALONE to true
   
  setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
   
  setting _R_CHECK_FORCE_SUGGESTS_ to true
   
  setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
   
-  using log directory 'C:/Users/USERzVBgqDuhyS/loon.Rcheck'
-  using R Under development (unstable) (2021-02-15 r80013)
-  using platform: x86_64-w64-mingw32 (64-bit)

## Downstream dependencies
