<script type="text/javascript">
window.onload = function() {
    document.getElementById("learn_R_tcltk").className += " selected";
    document.getElementById("learn_R").firstChild.href = "learn_R_tcltk.html";
    document.getElementById("learn_Tcl").firstChild.href = "learn_Tcl_Tk.html";
}
</script>


### A short overview of Tcl and R

A short comparison of the `R` and `Tcl` language is shown in the
following two sessions that do exactly the same.

<div class="twocolumn">

~~~~~ {.left}
# Session in R
a <- 3
x <- c(1,2,3)
b <- 4 + a + x[1]
factorial <- function(n) {
  if(n == 0) {
    return(1)
  } else {
    return(factorial(n-1)*n)
  }
}

print(factorial(7))
~~~~~

~~~ {.right .tclcode}
# Same Session in Tcl
set a 3
set x [list 1 2 3]
set b [expr {4 + $a + [lindex $x 0]}]
proc factorial {n} {
  if {$n eq 0} {
    return 1
  } else {
    return [expr {[factorial\
             [expr {$n-1}]]*$n}]
  }
}
puts [factorial 7]
~~~
</div>

The backslash `\` in `Tcl` can be used to write an expression on
multiple lines. A general introduction to `Tcl` written by Adrian
Waddell can be found
[here](https://math.uwaterloo.ca/statistics-and-actuarial-science/sites/ca.statistics-and-actuarial-science/files/uploads/files/Rtcltk_tcl.pdf). An
introduction to the `tcltk` `R` package by the package author Peter
Dalgaard's can be found found on
[page 27 in R News volume 1/3](http://cran.r-project.org/doc/Rnews/Rnews_2001-3.pdf)
and on 
[page 25 in R News volume 2/3](http://cran.r-project.org/doc/Rnews/Rnews_2002-3.pdf).

In `R`, `Tcl` code can be sent to the `Tcl` interpreter with the
`tcl()` and `.Tcl()` functions.

~~~~~
library(tcltk)
.Tcl('set a 1')
tcl('set','a',1)
~~~~~

Both functions return an `R` object (S3) with class `tclObj` which
displays itself with `<Tcl> 1` in the `R` console. In `Tcl`,
[everything is a string](http://wiki.tcl.tk/3018), which makes it easy
for `R` to display a `tcl` object by just adding a `<Tcl>` in front of
the string that represents the `Tcl` data structure.

Note that an object of class `tclObj` in `R` is just the return string
of a `Tcl` expression. So

~~~
x <- .Tcl('set a 1') 
.Tcl('set a 2')
~~~

will end with the `R` variable `x` to hold the string `<Tcl> 1` and
the `Tcl` variable `a` to hold the value `2`.

To create a `Tcl` variable that is accessible from `R` one can use the
`tclVar()` function as follows

~~~
a <- tclVar()
tclvalue(a) <- "Hello World"
.Tcl('info globals') # display all global variables in Tcl
.Tcl('puts $RTcl1')  # show the Tcl variable directly from the Tcl interpreter 
~~~

The `tclVar()` is the creator function whereas the `tclvalue()` is the
accessor function. The `tclVar()` function will create a `RTcl1`,
`RTcl2`, etc., global `Tcl` variable. `info globals` is a `Tcl`
command to display all global variables.

Since every `Tcl` object has a string representation the `tclvalue()`
accessor might not be as expected, as for example in

~~~
tclvalue(a) <- c(1,2,3) # set Tcl variable hold the list {1 2 3}
tclvalue(a)
~~~

the latter function will return `"1 2 3"`, the string representation
in `Tcl` for the list `[list 1 2 3]`. A workaround to this "quoting
problem" is to use `tclObj()` as an alternate accessor function in
combination with `as.character()`, `as.double()`, or `as.integer()`
function. For example, the following `R` command

~~~
as.integer(tclObj(a))
~~~

will return the desired `R` vector of length 3.

Data structures passed to the `tcl()` function will be converted with
`as.tclObj()` to a `Tcl` object. Valid `R` data structures that can be
converted to `Tcl` objects need a `storage.mode()` of one of
`character`, `integer`, `logical`, and `raw`. Other objects will throw
an error. This means vectors (also of length one) can be converted to
`Tcl` lists but other `R` data structures such as lists or data.frames
can not be converted to `Tcl` objects. Note that matrices and arrays
of numbers have a storage mode `double` and can hence be converted to
`Tcl` lists of dimension `1xn`. Here two valid examples

~~~
tcl('set','a', c(pi, 1, 2.4))
tcl('set','b', c(1, 2, "a"))
~~~

`R` functions can be called from the `Tcl` interpreter via callback
functions. For example

~~~
foo <- function(x,y) {
	x <- as.numeric(x)
	y <- as.numeric(y)
	print(x^2-y)
}
.Tcl.callback(foo)
~~~

will return the string `"R_call 0x16dca38 %x %y"`. Hence, to call
`foo()` from the `Tcl` interpreter one can run

~~~
.Tcl('R_call 0x16dca38 4 6')
~~~

Note that the `as.numeric()` conversation in `foo()` is necessary
since the function call from the `Tcl` interpreter passes strings as
arguments to `foo()`. Also, if `foo()` were to return an `R` object it
would be lost as the `R_call` will not convert `R` objects to `Tcl`
objects.

### A short overview of Tk and R

`Tk` is a graphical user interface toolkit. GUI elements are called
*widgets*. Widgets are arranged in a window with *geometry managers*
(such as `pack` and `grid`). Hence, the general workflow of creating a
GUI with `Tk` is to create *widgets*, *bind* them to events such as
mouse- or key- press and place them into a window.

A detailed introduction to the `tcltk` `R` package by the package
author Peter Dalgaard's can be found found on
[page 27 in R News volume 1/3](http://cran.r-project.org/doc/Rnews/Rnews_2001-3.pdf)
and on
[page 25 in R News volume 2/3](http://cran.r-project.org/doc/Rnews/Rnews_2002-3.pdf). A
good `Tk` overview with many examples can be found on
[TkDocs](http://www.tkdocs.com/).



The following `R` and `Tcl` code create the same GUI with a button
that says "Click Me" and when clicked will output "Hello World" to the
prompt.

<div class="twocolumn">

~~~~~ {.left}
# Session in R
tt <- tktoplevel()
b <- tkbutton(tt, text="Click Me")
tkpack(b, side = "top", anchor="w")
foo <- function() {
	print("Hello World")
}
tkconfigure(b, command=foo)
~~~~~

~~~ {.right .tclcode}
# Same Session in Tcl
set tt [toplevel .1]
set b [button .1.b -text "Click Me"]
pack $b -side top -anchor w
proc foo {} {
	puts "Hello World"
}
$b configure -command foo
~~~
</div>

Whereas the `Tcl` code comparison above did not show any conceptual
differences, the `Tk` part in `R` has some shortcuts that can be taken
via some wrapper functions (such as `tktoplevel()`) that are available
in the `tcltk` `R` package. These wrapper functions are supposed to
make the creation of `Tk` GUIs more natural for `R` users.

Every widget has one parent widget and possibly many children. Widgets
that can have children are usually used for layout, such as the
`frame` widget.

The child parent relationship is enforced in `Tk` by syntax. Every
widget is part of a *window path name* where parent and child are
separated by `.` (dot). The toplevel window itself is called `.`
(dot). So a widget path name might be `.a.b.c` where `c` is a child of
`b` is a child of `a` is a child of `.` (dot), the toplevel window.

The window path name is used to access and modify the state of a
widget. For example in the above `Tcl` session the `$b` gets
substituted with `.1.b` the window path name of the button widget.

In `R`, the explicit declaration of the window path name is omitted by
having `tktoplevel()`, `tkbutton()`, `tkframe()`, etc. creating its
own unique and valid path name. This results with having to pass the
parent widget or window to the widget creator functions
(`tktoplevel()` creates a window not a widget). Window path names,
however, still exist in `R` and can be seen when looking at `b$ID` in
`R`.

The `pack` geometry manager is explained in detail in
[this document](https://math.uwaterloo.ca/statistics-and-actuarial-science/sites/ca.statistics-and-actuarial-science/files/uploads/files/Rtcltk_geometry.pdf)
written by Adrian Waddell.

A description of all `Tk` widgets and their use can be found in the
[Tk 8.6 manual page](https://www.tcl.tk/man/tcl8.6/TkCmd/contents.htm). The
core widgets in the `Tk` packages are listed and displayed in
[this overview](http://wiki.tcl.tk/490).

The `tcltk` `R` package provides many wrapper functions for widget
commands such as the `configure` and `cget` which are valid sub
commands for every `Tk` widget. A list all functions provided by the
`tcltk` library including the wrapper functions can be obtained with

~~~
ls(name="package:tcltk")
~~~

If, for example, one wants to see the `Tcl` equivalent of an `R`
wrapper function, say the `tkwm.geometry()` function, one can often
see the `tcl()` call with

~~~
getAnywhere('tkwm.geometry')
~~~

which is in this case `tcl("wm", "geometry", ...)`, hence the
`geometry` sub-command of `wm`.
