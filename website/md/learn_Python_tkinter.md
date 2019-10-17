<script type="text/javascript">
document.getElementById("learn_Python_tkinter").className += " selected";
</script>

### A short overview of Tcl and Python

A short comparison of the `Python` and `Tcl` language is shown in the
following two sessions that do exactly the same.

<div class="twocolumn">

~~~~~ {.left}
# Session in Python
a = 3
x = [1,2,3]
b = 4 + a + x[0]
def factorial(n):
  if n == 0:
    return 1
  else:
    return n*factorial(n-1)



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
multiple lines.


# Tcl Interpreter

In python version < 3 the `Tkinter` package provides access to a `Tcl`
interpreter. In python version 3+ the package is called `tkinter` with
lower case.

~~~
import Tkinter as tk

root = tk.Tk()
root.eval("set tcl_version")
root.call("set", "tcl_version")
~~~

Also note the `tk._stringify` method.
