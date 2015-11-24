

Installation
============

Append the path to the `pkgIndex.tcl` to the `auto_path`
variable. Then load `loon` with

~~~
package require loon
~~~


## With the load.tcl script

You can start load the `loon` package and the `olive` and `iris` data
used in the *learn* section of the
[web manual](http://waddella.github.io/loon/learn_Tcl_intro.html) by
running the following command in the terminal (make sure to set your
working directory accordingly)

~~~
tkcon load.tcl
~~~

then in the `tkcon` console you can start creating plots


~~~
set p [plot -x $oleic -y $stearic -color $Area -xlabel oleic -ylabel stearic -showScales TRUE]
~~~


## Manually 


Assume that the current working directory is set to the folder that
contains the `pkgIndex.tcl` file. Then, start `tclsh` or better
`tkcon` and enter

~~~
lappend auto_path [pwd]

package require loon

namespace import loon::*

set p [plot -x {1 2 3 4} -y {3 2 1 4} -color red]
~~~

For more information please read the *learn* section in the
[web manual](http://waddella.github.io/loon/learn_Tcl_intro.html).
