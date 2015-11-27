<script type="text/javascript">
document.getElementById("develop").className += " selected";
document.getElementById("develop_setup").className += " selected";
</script>


### Introduction

The information on this page is useful if you wish to modify or
contribute to the `loon` project. The development tools we use are
`git` and `make` to automate some tasks.

You may find it useful to browse through the book
[Tcl/Tk: A Developer's Guide](http://www.amazon.com/Tcl-Third-Edition-Engineering-Programming/dp/0123847176/).
We loosely followed the books suggestion on how to make a
megawidget. Generally this book is an excellent source of information
on `Tcl/Tk` programming.


### Get source

All `loon` related projects such as the `Tcl` and `R` package are
saved in one repository on `github`. There is a directory for each
programming language we have defined bindings.

Download the repository using

~~~
git clone http://github.com/waddella/loon.git
~~~

We have created some make rules to automate some tasks:

~~~
make Tcl
make R
make website
make debug85
make debug86
~~~

### Contributing


### Current issues

- Also `l_aspect('.1.plot') <- 1` (string instead of a variable as an
  argument) will not work because of the way replacement functions are
  implemented in `R`. I.e. the call `l_aspect('.1.plot') <- 1` should
  be equivalent to

		`*tmp*` <- ".1.plot"
		".1.plot" <- "l_aspect<-"(`*tmp*`, value=1)
		rm(`*tmp*`)

	but somehow `R` thinks `".1.plot"` is a variable (maybe).




