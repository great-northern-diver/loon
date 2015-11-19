
<script>
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
* If you experience difficulties with `loon` use the
  [Issue tracker on github](https://github.com/waddella/loon/issues).
* If you have difficulties with exporting images with the `l_export`
  function then yake screenshots:
	  * On OX press Command + Shift + 4 and then press space and
		select the window
	  * On Linux install a program called [Shutter](http://shutter-project.org/)
	  * For Windows use [Greenshot](http://getgreenshot.org/)


# Installation

First try to install the suggested packages so that you can run all
the demos. Note that these packages are not needed to install `loon`,
they are just nice to have to run the demos.

~~~
source("https://bioconductor.org/biocLite.R")
biocLite(c('graph', 'Rgraphviz', 'RDRToolbox'))
install.packages(c('maps', 'sp', 'RColorBrewer', 'rworldmap',
	'rgl', 'RnavGraphImageData','dplyr'))
install.packages('PairViz', 'scagnostics', 'kernlab', 'devtools', 'testthat')
~~~

* Windows user need to install Tcl and Tk version 8.6 and link it
  against your `R`. [Read below](#linking-activetcl-with-r-on-windows).
* Mac users need to install [XQuartz](https://cran.r-project.org/bin/macosx/)


## With devtools from github

~~~
library(devtools)

install_github("waddella/loon", subdir="R")
~~~

You can switch between development version and the version you have
installed manually

~~~
# use development version of loon
dev_mode(on=TRUE)

# switch back to stable version of loon 
dev_mode(on=FALSE)
~~~


## Manually with tar.gz package

In Rstudio, select Packages, Install, Install from: Package Archive
File (.tar.gz), select the 'loon_0.8.x.x.tar.gz' file and press the
install button.

![Install loon in Rstudio](images/install_rstudio.png "install loon with Rstudio.")


To install the `loon` `R` package as usual the following code in your
terminal

~~~
R CMD INSTALL loon_0.9.tar.gz
~~~


# Linking ActiveTcl with R on Windows


I put a screencast with these instructions
[onto youtube here](https://www.youtube.com/watch?v=2PsVBYNftrU).


Unfortunately `R` on windows still ships with `Tcl` version 8.5 and
`loon` requires `Tcl` version 8.6. Hence, you need to install `Tcl`
version 8.6 and link it with `R`.

In `R`, enter

~~~
.Machine$sizeof.pointer
~~~

If the return value is `8` you run a 64 bit build of `R` and if the
return value is `4` you run a 32 bit build of `R`. Download the
ActiveTcl version 8.6.x with the same build architecture:

* If your `R` is a 64 bit build [download Windows (64-bit, x64) Tcl 8.6.x](http://www.activestate.com/activetcl/downloads/thank-you?dl=http://downloads.activestate.com/ActiveTcl/releases/8.6.4.1/ActiveTcl8.6.4.1.299124-win32-x86_64-threaded.exe)
* If your `R` is a 32 bit build [download Windows (x86) Tcl 8.6.x](http://www.activestate.com/activetcl/downloads/thank-you?dl=http://downloads.activestate.com/ActiveTcl/releases/8.6.4.1/ActiveTcl8.6.4.1.299124-win32-ix86-threaded.exe)

Install ActiveTcl under `C:\Tcl64` or `C:\Tcl32`. You can install both versions if you switch between 64 bit and 32 bit `R`.

In the `C:/Tcl64/bin` and/or `C:/Tcl32/bin` folder copy paste the
`tk86.dll` and `tcl86.dll` files (create duplications) and rename them
to `tk85.dll` and `tcl85.dll`, respectively.

Create or modify the `.Rprofile` file at the `C:/Users/<your
username>/.Rprofile` to include the code below. To create the
`.Rprofile` use Rstudio and use the menu to create a new Text File.

~~~
.First.sys()
if (.Machine$sizeof.pointer == 8) {
  Sys.setenv("MY_TCLTK"="C:/Tcl64/bin")
} else {
  Sys.setenv("MY_TCLTK"="C:/Tcl32/bin")
}
~~~

Restart R (i.e. Rstudio) and check if the above steps were successful
by entering the following code in `R`

~~~
Sys.getenv("MY_TCLTK")
~~~

This should either return `C:/Tcl32/bin` or `C:/Tcl64/bin`. If this
works enter the following code in `R`

~~~
library(tcltk)
tcl("set", "tcl_version")
~~~

If this returns `8.6` you were successful. Otherwise follow the steps
in my
[youtube instructions](https://www.youtube.com/watch?v=2PsVBYNftrU).

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
you have the experience to fix this then please
[contact me](mailto:adrian@waddell.ch).


# More Important Notes

<!-- If you are using this version of `loon` you have volunteered to be a beta
tester. Please report typos and bugs to `adrian@waddell.ch`.-->

We may change the syntax during the beta testing phase without
maintaining backward compatibility. We try to maintain list with the
syntax changes on this page.

<div class="todo">

Note that features that are highlighted with pastel red are not
available in the beta version of `loon`.

</div>

Also note that only part of `loon`'s functionality is documented
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

- There is no support for legends
- Most images showing `loon`'s plots are anti-aliased on this
  webpage. You will probably not see anti-aliased plots on your screen
  because the `canvas` widget is only anti-aliased under `OS X`. Also,
  the `Tcl` version that is included in `R` under `OS X` does not have
  an anti-aliased canvas.

# Also good to know

- currently only the `ps` file format works reliably for image
 exports. I recommend for now to **make screenshots** if you need to
 put a `loon` plot into your report.

