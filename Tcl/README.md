

INSTALLATION
============

Append the path to the `pkgIndex.tcl` to the `auto_path`
variable. Then load `loon` with

~~~
package require loon
~~~


For example, assume that the current working directory is set to the
folder that contains the `pkgIndex.tcl` file. Then, start `tclsh` or
better `tkcon` and enter

~~~
lappend auto_path [pwd]

package require loon

namespace import loon::*

set p [plot -x {1 2 3 4} -y {3 2 1 4} -color red]
~~~

For more information please read the *learn* section in the
[web manual](http://waddella.github.io/loon/learn_Tcl_intro.html).
