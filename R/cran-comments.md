## Test environments  1.3.5

* local OSX, Big Sur 11.2.1,  R 4.0.2
* Ubuntu Xenial 16.04  (on travis-ci)
* check_win_devel 
  - using R Under development (unstable) (2021-03-14 r80087)
  - using platform: x86_64-w64-mingw32 (64-bit)
  - using session charset: ISO8859-1
  
* Using

  `devtools::check_rhub(env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))`
  
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit ... OK
  - linux-x86_64-fedora-clang & Ubuntu ... PREPERROR:
  
    Errors involve not finding bioconductor files and tcktk not loading
    
    
     Error : Bioconductor version '3.13' requires R version '4.1'; 
    
     R version is too new; see https://bioconductor.org/install`
    
  
     AND 
   
     library(loon)
   
     Loading required package: tcltk
   
     Error: package or namespace load failed for ‘loon’:
   
     .onLoad failed in loadNamespace() for 'loon', details:
   
     call: structure(.External(.C_dotTcl, ...), class = "tclObj")
   
     error: [tcl] no display name and no $DISPLAY environment variable.
   
     In addition: Warning message:
   
     no DISPLAY variable so Tk is not available
   
     Execution halted`
  
  

## R CMD check results

0 errors
0 warnings
1 NOTE about maintainer

  ... NOTE
  Maintainer: 'R. Wayne Oldford <rwoldford@uwaterloo.ca>'

