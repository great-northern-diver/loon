
# Getting started

The [`tkinter`](https://docs.python.org/3/library/tkinter.html)
package provides an interface between `Python` and `Tcl/Tk`. In the
following code I assume that the Python version 3+ is used. (For
Python 2.x the package is called
[`Tkinter`](https://docs.python.org/2/library/tkinter.html)).


First the tkinter package needs to be loaded

```
import tkinter
```

Then load the tk library which creates the toplevel window `.`. The
toplevel window needs to be hidden because all other `Tk` windows will
be children of it, and if the toplevel window gets destoyed (closed)
then all its children get destroyed too.

```
tk = tkinter.Tk()
tk.withdraw() 
```

Now load the loon library (needs to be done once, replace the path
accordingly)

```
tk.eval('lappend auto_path /home/adrian/Desktop/loon/Tcl')
tk.eval('package require loon')
```

The `tk.eval` method provides access to the `Tcl` interpreter

Now you can create the first `loon` plot.

```
tk.eval('set p [loon::plot -x {1 2 3} -y {1 2 3}]')
```

Alternatively you can also import the loon namespace in `Tcl`

```
root.eval('namespace import loon::*')
root.eval('set p [plot -x {1 2 3} -y {1 2 3}]')
```

# Roadmap

Look at the Python Tix package.







