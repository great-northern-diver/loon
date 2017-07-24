
<script type="text/javascript">
document.getElementById("beta").className += " selected";
</script>

---
title: "install loon"
---

Note that this page provides installation instructions for R. If you
want to install `loon` for `Tcl` then follow the instructions
[here](https://github.com/waddella/loon/tree/master/Tcl).

[TclWindows]: linkingActiveTcl.html
[rstudio]: http://www.rstudio.com/
[CRAN]: http://cran.r-project.org/

# Important

* It is best to use `loon` in [RStudio][rstudio] or within a terminal.
	* The Rgui app on OSX does not work well with `loon`.
	* The default Windows `R` GUI will not accept debug messages from
      `Tcl`, so for now you need to use [RStudio][rstudio] or the
      terminal.
* Mac users need to install [XQuartz](http://www.xquartz.org)
	* Do not close `XQuartz` while `R` is running! Otherwise you end
      up crashing the active `R` session (including `RStudio`).
* On Ubuntu one should install the `libtk-img` debian package
  (e.g. with `sudo apt-get install libtk-img`) in order to get a wide
  variety of export formats with the `l_export` function.


## Further Notes

* If you experience difficulties with `loon` use the
  [Issue tracker on github](https://github.com/waddella/loon/issues).
* If you have difficulties with exporting images with the `l_export`
  function then take screenshots:
	  * On OSX press Command + Shift + 4 and then press space and
		select the window
	  * On Linux install a program called [Shutter](http://shutter-project.org/)
	  * For Windows use [Greenshot](http://getgreenshot.org/)
* If you need to install `loon` manually on a Windows machine with an
  R version that is less than `3.4.0` then you need install `Tcl`
  version 8.6 and link R against it, see the
  [instructions here][TclWindows].

# Installation

## Installing the loon package

The `loon` package is on
[CRAN](https://cran.r-project.org/web/packages/dplyr/index.html)
available. To install the package start your `R` and run

~~~{r}
install.packages('loon')
~~~

You can also install the latest development release directly from
GitHub with the following R code

~~~
devtools::install_github("waddella/loon", subdir="R")
~~~


<!--
### From a local file

In Rstudio, select Packages, Install, Install from: Package Archive
File (.tar.gz), select the 'loon_1.0.1.tar.gz' file and press the
install button.

![Install loon in Rstudio](images/install_rstudio.png "install loon with Rstudio.")


To install the `loon` `R` package as usual the following code in your
terminal

~~~
R CMD INSTALL loon_1.0.0.tar.gz
~~~

-->

## Packages used for Examples and Demos

The following packages are used in `loon`'s examples and demos. Note
that these packages are not needed to install `loon`, they are just
nice to have to run all the examples and demos. You can skip this step
and
[check if your `loon` installation was successful.](#check-if-your-installation-was-successful)


We split the code to install the suggested packages into four sections
as not all the packages are easy to install.

First, these packages from [CRAN][CRAN] should install without any
issues

~~~
install.packages(c('maps','sp','RColorBrewer',
    'RnavGraphImageData','rworldmap', 'scales'))
~~~

The following packages on [Biocoductor](http://www.bioconductor.org/)
should also install without any issues

~~~
source("https://bioconductor.org/biocLite.R")
biocLite(c('graph', 'RDRToolbox', 'Rgraphviz'),
    suppressUpdates=TRUE, suppressAutoUpdate=TRUE)
~~~


The following packages on [CRAN][CRAN] have dependencies that might need
need special care

~~~
install.packages(c('dplyr', 'rgl', 'PairViz', 'scagnostics', 'kernlab',
    'testthat', 'knitr', 'rmarkdown'))
~~~



# Check if your Installation was Successful

In `R` enter

~~~
library(loon)

p <- with(iris, l_plot(x=Sepal.Length, y=Sepal.Width, color=Species))
~~~

If this creates a scatterplot and an inspector loon was correctly
installed. Follow the steps in the [UI section](UI.html) and the
[Learn section](learn_R_intro.html) to learn how to use `loon`.


# Fast Image Resizing for Image Point Glyphs

On Linux and OS X it is advisable to install the
[ImageScale Tcl extension](https://github.com/waddella/tclImageScale)
for fast image resizing. `loon` will use the compiled `C` code for
image resizing when available.

The TEA setup of `ImageScale` for Windows does currently not work. If
you know how to change the `makefile.vc` in the
[win folder](https://github.com/waddella/tclImageScale/tree/master/win)
so that the `ImageScale` package also compiles under Windows then
please [contact me](mailto:adrian@waddell.ch).


# More Important Notes

Note that only part of `loon`'s functionality is documented on this
webpage. However, most of `loon`'s features are used in the `R`
documentation manuals, `R` package demos and `R` package
vignettes.

## Manual
To get an index of all help entries run

~~~
help(package = "loon")
~~~


## Package Demos
To get a listing of all of `loon`'s demos enter

~~~
demo(package="loon")
~~~

Run a particular demo as follows

~~~
demo("l_timeseries")
~~~

To get the location of the source code of a particular demo
(e.g. `l_timeseries`) use

~~~
system.file("demo", "l_timeseries.R", package = "loon")
~~~

## Package Vignettes

To get a list of all package vignettes run

~~~
vignette(package = "loon")
~~~

To open the `minority` vignette run

~~~
vignette('minority', package = "loon")
~~~


# Good to know

- There is presently no support for legends
- Most images showing `loon`'s plots are anti-aliased on this
  webpage. You will probably not see anti-aliased plots on your screen
  because the `canvas` widget is only anti-aliased under `OS X`. Also,
  the `Tcl` version that is included in `R` under `OS X` does not have
  an anti-aliased canvas.
- Except for Linux with the `libtk-img` package installed (e.g. `Img`
 `Tcl` extension): currently only the `ps` file format works reliably
 for image exports (although the font mapping is not correct yet). I
 recommend for now to make screenshots if you need to put a `loon`
 plot into your report.

