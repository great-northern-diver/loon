
<script type="text/javascript">
document.getElementById("beta").className += " selected";
</script>

---
title: "beta tester instructions - loon"
---

# Important

* Use `loon` in [Rstudio](https://www.rstudio.com/) or within a
  terminal.
	  * The Rgui app on OSX does not work well with `loon`.
	  * The default Windows `R` GUI will not accept debug messages
        from `Tcl`, so for now you need to use Rstudio.
* Windows users need to install `Tcl` version 8.6, see the
  [instructions below](#linking-activetcl-with-r-on-windows).
  * Mac users need to install [XQuartz](https://cran.r-project.org/bin/macosx/)
	* Do not close `XQuartz` while `R` is running! Otherwise you end
      up crashing the active `R` session (including `RStudio`).
* If you experience difficulties with `loon` use the
  [Issue tracker on github](https://github.com/waddella/loon/issues).
* If you have difficulties with exporting images with the `l_export`
  function then take screenshots:
	  * On OSX press Command + Shift + 4 and then press space and
		select the window
	  * On Linux install a program called [Shutter](http://shutter-project.org/)
	  * For Windows use [Greenshot](http://getgreenshot.org/)


# Installation

* Windows users need to first install `Tcl` and `Tk` version 8.6 and
  link it against your
  `R`. [Read below](#linking-activetcl-with-r-on-windows).
* Mac users need to first install
  [XQuartz](https://cran.r-project.org/bin/macosx/)

## Installing the loon package

<!-- ### Fom github -->

~~~
library(devtools)

install_github("waddella/loon", subdir="R")
~~~

`loon` is under active development. You can also **update** the `loon`
package with the above code.


<!--
You can switch between development version and the version you have
installed manually

~~~
# use development version of loon
dev_mode(on=TRUE)

# switch back to stable version of loon 
dev_mode(on=FALSE)
~~~
-->

<!--
### From a local file

In Rstudio, select Packages, Install, Install from: Package Archive
File (.tar.gz), select the 'loon_1.0.0.tar.gz' file and press the
install button.

![Install loon in Rstudio](images/install_rstudio.png "install loon with Rstudio.")


To install the `loon` `R` package as usual the following code in your
terminal

~~~
R CMD INSTALL loon_1.0.0.tar.gz
~~~

-->

## Packages used for Demos

The following packages are used in `loon`'s package demos. Note that
these packages are not needed to install `loon`, they are just nice to
have to run the demos. We split the code to install packages into four
sections as not all packages are easy to install.

First, these packages from CRAN should install without any issues

~~~
install.packages(c('maps', 'sp', 'RColorBrewer', 'dplyr',
    'RnavGraphImageData', 'rworldmap'))
~~~

The following packages on Biocoductor should also install without any
issues

~~~
source("https://bioconductor.org/biocLite.R")
biocLite(c('graph', 'RDRToolbox'), suppressUpdates=TRUE, suppressAutoUpdate=TRUE)
~~~

The `Rgraphviz` package is sometimes not easy to install

~~~
biocLite('Rgraphviz', suppressUpdates=TRUE, suppressAutoUpdate=TRUE)
~~~

The following packages on CRAN have also dependencies that need
special care. Make sure that you have 

~~~
install.packages(c('rgl', 'PairViz', 'scagnostics', 'kernlab'))
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

We may change the syntax during the beta testing phase without
maintaining backward compatibility. We will maintain a list with the
syntax changes on this page.

Note that only part of `loon`'s functionality is documented
here. However, most of `loon`'s features are used in the `R` package
demos. To get a listing of all of `loon`'s demos enter

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

# Good to know

- There is presently no support for legends
- Most images showing `loon`'s plots are anti-aliased on this
  webpage. You will probably not see anti-aliased plots on your screen
  because the `canvas` widget is only anti-aliased under `OS X`. Also,
  the `Tcl` version that is included in `R` under `OS X` does not have
  an anti-aliased canvas.
- currently only the `ps` file format works reliably for image exports
 (although the font mapping is not correct yet). I recommend for now
 to make screenshots if you need to put a `loon` plot into your
 report.

