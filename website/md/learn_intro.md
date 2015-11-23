<script>
document.getElementById("learn_intro").className += " selected";
</script>

---
title: learn loon
---

# Introduction

<Tcl> `loon` is written in `Tcl` and `Tk`. Users familiar with `Tk`
widgets will feel familiar with the `loon` API as the `loon` widgets
follow the standard `Tk` conventions. </Tcl>


<R>

<!--

<div class="todo">`loon` is available in `R` with the `loon` `R`
package which is hosted on `CRAN`.</div> 

-->

`loon` is written in `Tcl` and `Tk`, and the `loon` `R` package
provides a thin layer of `R` code to embed `loon` into the `R`
environment.. `Tcl` is a programming language and `Tk` is a GUI
toolkit written as an extension for `Tcl`.

Knowledge of `Tcl` and `Tk` are not required to use `loon` in
`R`. However, a basic understanding of `Tcl` and `Tk` helps to
appreciate certain API design decisions and, it also helps to follow
some advanced topics such as event bindings and widget layout with
geometry management. Hence, you may read
[our short overview of `tcltk` `R` package](learn_R_tcltk.html).

If your knowledge of `R` is generally basic, then consider reading the
slides from the
[R introduction course](http://adrian.waddell.ch/RforEcon/) written by
Adrian Waddell.

</R>

<Python> `loon` is written in `Tcl` and `Tk`. Users unfamiliar with
the language and toolkit may read <a
href="learn_Python_tkinter.html">our short overview of Python &
tkinter</a>.  </Python>



# Installation
<Tcl>
Get package from github

~~~{.todo}
git clone https://github.com/waddella/loon.git
~~~

To compile the package change the working directory to the `Tcl`
subfolder and run

~~~
./configure
make
~~~

Optionally install the package

~~~
make install
~~~

If you do not install the package you need to add the package path to
`auto_path` variable in the `tcl` console

~~~
lappend auto_path path_to_foler_containing_pkgIndex.tcl
~~~
</Tcl>

<R>

See the [beta section](beta.html).

<!--

To install the newest stable version of `loon` from CRAN run the
following command in an R session

~~~{.todo}
install.packages("loon")
~~~

Development versions can be installed and tested from github with the
`devtools` `R` package as follows

~~~{.todo}
library(devtools)

install_github("loon")

# use development version of loon
dev_mode(on=TRUE)

# switch back to stable version of loon
dev_mode(on=FALSE)
~~~

-->

</R>



# Getting Started

<Tcl>

In the `Tcl` console (e.g. `tclsh`, `wish`, `tkcon`) load the `loon`
package with

~~~
package require loon
~~~

If you need to import images from `jpeg` and `png` files you also need
to load the `Img` package

~~~
package require Img
~~~

Optionally, if you want to import all of `loon`'s commands into the
global namespace, run

~~~
namespace import loon *
~~~

If you do not import `loon`'s procedures into the global namespace
then you need to prepend `loon::` in front of `loon`'s procedure names
(e.g. `loon::plot` instead of `plot`).


An example scatterplot is created with

~~~
plot -x {4 1 5} -y {2 3 1} -color {red orange blue} -size {1.4 5.2 4}
~~~

If you want to use `loon` interactively via the `tcl` shell take a
look at the `tk` console [`tkcon`](http://tkcon.sourceforge.net/).
</Tcl>


<R> The following `R` code provides a short `loon` sample session

~~~
# Load the library
library("loon")

# Create a scatterplot
p <- with(iris, l_plot(x=Sepal.Width, y=Sepal.Length, color=Species,
  linkingGroup="iris"))

# Create a histogram
h <- l_hist(x=iris$Petal.Width, linkingGroup="iris")

# Query a plot state
p['color']

# Modify a plot state
p['size'] <- iris$Petal.Length
~~~
</R>

The user interfaces of `loon`'s displays are explained in the
[UI section](UI.html).


<R>

# Package Demos

The `loon` `R` package comes with many demos that show how to use
particular parts of the `loon` API. See,

~~~
demo(package = "loon")
~~~

# Re-creating Object Handles


It is possible to re-create any `loon` object handle in an `R`
session. That is, handles for displays, layers, glyphs, navigators and
context handles can be created with the widget path name and the
appropriate ids. The object handle can then be used with the methods
`[` and `[<-` to access and modify states. For example, for a display with the widget path name `.l1.hist` one can create a `loon` plot handle as follows

~~~
h <- '.l1.hist'
class(h) <- 'loon'  
~~~

For a layer with the layer id `layer23` of that display the layer
handle can be created as follows

~~~
l <- 'layer23'
class(l) <- c('loon', 'l_layer')
attr(l, 'widget') <- '.l1.hist' 
~~~

For a context with the id `context0` of a navigator with id `navigator1` of a graph with widget path name `.l4.graph`, the context handle is created as follows:

~~~
con <- 'context0'
class(con) <- c('loon', 'l_context')
attr(con, 'widget') <- '.l4.graph'
attr(con, 'navigator') <- 'navigator1' 
~~~

and so on. The `l_cget` and `l_configure` have `target` as their first argument which either accepts a `loon` object handle or a vector with the widget path name and the object ids. The following code blocks have each two lines that do the same

~~~
l_configure(h, color='red')
l_configure('.l1.hist', color='red')
~~~

~~~
l_configure(l, color='green')
l_configure(c('.l1.hist', 'layer23'), color='green')
~~~

~~~
l_configure(con, command='')
l_configure(c('.l4.graph', 'navigator1', 'context0'), command='')
~~~

The re-creation of object handles is useful when, for example, an object handle is lost or overwritten.



# A Couple of Notes

`loon`'s `l_plot` and `l_hist` functions are similar to use as `R`'s
base graphic function `plot`. This is because we use the `R` function
`xy.coords` to extract the coordinates from `x` and `y`. For example
for

~~~
l_plot(c(1,2,3), c(4,2,4), cex=c(4,1,8), col=c('red','blue','yellow'))
with(mtcars, l_plot(hp ~ wt))
~~~

the `l_plot` function call could be replaced with `plot` for non-interactive equivalents using `R`'s base graphics. Note that `loon` will generate warnings if you use some base `R` graphics argument names instead of `loon`'s state names (e.g. `col` vs `color` or `lwd` vs `linewidth`).

Also, 

- Note that in `l_plot` the arguments `x` and `y` are the only
arguments that are allowed to be unnamed. All other arguments must be
named. See `args(l_plot)`.

- For `loon` plots, the arguments `cex`, `col`, `xlab`, `ylab` and
`lwd` get always internally changed to `size`, `color`, `xlabel`,
`ylabel` and `linewidth`, respectively. We recommend to use the long
version as they represent the plot state names.

- To save a `loon` plot use the `l_export` function. For any format
  other than `ps` and `pdf` you need the `Img` `Tcl` package to be
  loaded. 

- Note that in `R` consecutive `plot` calls write their output to the same device (e.g. window), but in `loon` consecutive `l_plot` calls will open a new window for each call. If an existing `loon` plot should be modified use the `l_configure` function with an `x` and/or `y` argument. For example,

		p <- l_plot(iris[,1:2], color=iris$Species, showScales=TRUE)
		l_configure(p, x=mtcars$hp, y=mtcars$mpg, color=mtcars$gear,
			ylabel='Gross horsepower', ylabel='Miles/(US) gallon')
		l_scaleto_world(p)

</R>

