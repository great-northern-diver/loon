Note passes all tests on all platforms EXCEPT Unix platforms provided by 

devtools::check_rhub()

There the FAILURE is due to inability of the Unix VM to install (suggested) Bioconductor packages, 

AND most frustratingly, its failure to successfully load the `tcltk` package since it cannot set the `DISPLAY` variable.

Both of these are beyond my control I think.

Results are as summarized below.


## Test environments  1.3.8

* local OSX, Big Sur 11.5.2,  R 4.1.0  - check as cran -> SUCCESS
* check_win_devel 
  * using log directory 'd:/RCompile/CRANguest/R-devel/loon.Rcheck'
  * using R Under development (unstable) (2021-09-17 r80929)
  * using platform: x86_64-w64-mingw32 (64-bit)
  * using session charset: ISO8859-1
  
  SUCCESS 
  
* check_rhub

  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  
    1 NOTE  ... seems to notice BioConductor suggested packages.
    * checking package dependencies ... NOTE
    
      Packages suggested but not available for checking:
      
        'graph', 'Rgraphviz', 'RDRToolbox'
    
   * Fedora Linux, R-devel, clang, gfortran
   
     PREPERROR
   
     Status: error
     
     Many, many, ..., many instances of the following error:
     
     328#> Error : Bioconductor does not yet build and check packages for R version 4.2; see
     
     329#> https://bioconductor.org/install
     
     
     Ending with bioconductor error again, and finally an error installing `tcl` 
     
     4082#> installation of package ‘PairViz’ had non-zero exit status
     
     4083#> Error : Bioconductor does not yet build and check packages for R version 4.2; see
     
     4084#> https://bioconductor.org/install
     
     4085#> > library(loon)
     
     4086#> Loading required package: tcltk
     
     4087#> Error: package or namespace load failed for ‘loon’:
     
     4088#> .onLoad failed in loadNamespace() for 'loon', details:
     
     4089#> call: structure(.External(.C_dotTcl, ...), class = "tclObj")
     
     4090#> error: [tcl] no display name and no $DISPLAY environment variable.
     
     4091#> In addition: Warning message:
     
     4092#> no DISPLAY variable so Tk is not available
     
     4093#> Execution halted

     4094#> Build step 'Execute shell' marked build as failure


    
   * Ubuntu Linux 20.04.1 LTS, R-release, GCC
   
     PREPERROR
   
     Status: error
     
     None of the bioconductor errors mentioned in Fedora.  Though some
     mention of getting repositories right vis-a-vis Bioconductor.
     
     FAILS at end with the following lines:
     
     7599#> 'getOption("repos")' replaces Bioconductor standard repositories, see

     7600#> '?repositories' for details

     7601#> replacement repositories:
     
     7602#> CRAN: https://cloud.r-project.org
     
     7603#> > library(loon)
      
     7604#> Loading required package: tcltk
     
     7605#> Error: package or namespace load failed for ‘loon’:
     
     7606#> .onLoad failed in loadNamespace() for 'loon', details:
     
     7607#> call: structure(.External(.C_dotTcl, ...), class = "tclObj")
     
     7608#> error: [tcl] no display name and no $DISPLAY environment variable.
     
     7609#> In addition: Warning message:
     
     7610#> no DISPLAY variable so Tk is not available
      
     7611#> Execution halted
     
     7612#> Build step 'Execute shell' marked build as failure
     
     7613#> Pinging https://builder.r-hub.io/build/FAILURE/loon_1.3.8.tar.gz-eae67e9ce06f4eb9b0cb71074326694f/2021-09-21T20:43:55Z
     
     7614#> {"status":"ok"}
     
     7615#> Finished: FAILURE
     
     
     
     

