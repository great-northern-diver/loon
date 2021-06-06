## Test environments  1.3.7

* local OSX, Big Sur 11.2.3,  R 4.0.5  - check as cran -> SUCCESS
* Ubuntu Xenial 16.04  (on travis-ci)  - SUCCESS
* check_win_devel 
  - using R Under development (unstable) (2021-06-01 r80444)
  - using platform: x86_64-w64-mingw32 (64-bit)
  - using session charset: ISO8859-1
  
  SUCCESS 
  
* check_rhub

  `devtools::check_rhub(env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))`

  `PREPERRORS` appear beyond my control
  
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  
    Status: success 
    
    Duration: 34.1 seconds
  
    https://builder.r-hub.io/status/loon_1.3.7.tar.gz-dae2cae7379842bcbd9c3296861eb0ff
    
    
    ```
    Error: Bioconductor does not yet build and check packages for R version 4.2; see
  
    https://bioconductor.org/install
    
    Execution halted
    ```

  - Build ID:   loon_1.3.7.tar.gz-0892fccf79014aeea86eb68aeefef49f
   
    Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC
  
    Errors involve not finding bioconductor files and tcktk not loading
    
    ```
    8860#> > library(loon)
    8861#> Loading required package: tcltk
    8862#> Error: package or namespace load failed for ‘loon’:
    8863#> .onLoad failed in loadNamespace() for 'loon', details:
    8864#> call: structure(.External(.C_dotTcl, ...), class = "tclObj")
    8865#> error: [tcl] no display name and no $DISPLAY environment variable.
    8866#> In addition: Warning message:
    8867#> no DISPLAY variable so Tk is not available
    8868#> Execution halted
    ```
  
  - Build ID:	loon_1.3.7.tar.gz-25dece826340466ebc15f2614c4082bd
    
    Platform:	Fedora Linux, R-devel, clang, gfortran
    
    Missing `rJava` and `Bioconductor` 
  
    ```
    2735#> checking Java support in R... present:
    2736#> interpreter : '/usr/bin/java'
    2737#> archiver : ''
    2738#> compiler : ''
    2739#> header prep.: ''
    2740#> cpp flags : ''
    2741#> java libs : ''
    2742#> configure: error: Java Development Kit (JDK) is missing or not registered in R
    2743#> Make sure R is configured with full Java support (including JDK). Run
    2744#> R CMD javareconf
    2745#> as root to add Java support to R.
    2746#> If you don't have root privileges, run
    2747#> R CMD javareconf -e
    2748#> to set all Java-related variables and then install rJava.
    2749#> ERROR: configuration failed for package ‘rJava’
    2750#> * removing ‘/home/docker/R/rJava’
    2751#> Error : Bioconductor does not yet build and check packages for R version 4.2; see
    2752#> https://bioconductor.org/install
    ```
    
    And then `tcltk` loading and `DISPLAY` variable as in previous Unix
    
    

## R CMD check results on Mac (see top of file)

0 errors
0 warnings
1 NOTE about maintainer

  ... NOTE
  Maintainer: 'R. Wayne Oldford <rwoldford@uwaterloo.ca>'

