
Installation
============

`Loon` requires `Tcl` and `Tk` version `8.6`. In order to use the
`loon` package the `Tcl` interpreter needs to find `loons`'s
`pkgIndex.tcl` file. `Tcl` searches for packages in all the paths
listed in the `auto_path` `Tcl` variable. If you start `tclsh`, `wish`
or `tkcon` you can look at the `auto_path` variable with

~~~
puts $auto_path
~~~

From now on, I will only mention `tkcon`, but note that you can also
use `tclsh` or `wish` instead.

Here are three different approaches to have the `Tcl` interpreter find
`loon`'s `pkgIndex.tcl` file:

* Manually add the path to `loon`'s `pkgIndex.tcl` file to the
`auto_path` variable. In `tkcon` enter

		lappend auto_path "path-to-parent-directory-of-pkgIndex.tcl"

	this approach requires you to be repeated every time you start a
    new `tkcon` instance. The next two approaches do not require to be
    repeated when starting a new `Tcl` interpreter.

* Copy the `loon` folder to a path that is listed in the `auto_path`
variable.

* Add the path to `loon`'s `pkgIndex.tcl` file to the `TCLLIBPATH`
  environment variable, e.g. (form the bash)

		export TCLLIBPATH="$TCLLIBPATH /Users/arwaddel/Desktop/loon/Tcl"

Then, to load `loon` enter the following in `tkcon`

~~~
package require loon
~~~

And create a scatterplot with

~~~
dict with ::loon::data::olive {
    set p [loon::plot -x $oleic -y $stearic -color $Area \
        -xlabel oleic -ylabel stearic]
}
~~~

For more information please read the *learn* section in the
[web manual](http://great-northern-diver.github.io/loon/l_help/learn_Tcl_intro.html).


# Fast Image Resizing for Image Point Glyphs

On Linux and OS X it is advisable to install the
[ImageScale Tcl extension](https://github.com/waddella/tclImageScale)
for fast image resizing. `loon` will use the compiled `C` code for
image resizing when available.

The TEA setup of `ImageScale` for Windows does currently not work. If
you know how to change the `makefile.vc` in the
[win folder](https://github.com/waddella/tclImageScale/tree/master/win)
so that the `ImageScale` package also compiles under Windows then
please [contact Adrian Waddell](mailto:adrian@waddell.ch).

