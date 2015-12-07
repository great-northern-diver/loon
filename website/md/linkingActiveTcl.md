# Linking ActiveTcl with R on Windows


We put a screencast with these instructions
[onto youtube here](https://www.youtube.com/watch?v=2PsVBYNftrU). Note
that you need to install both 32bit and 64 bit Tcl.


Unfortunately `R` on Windows still ships with `Tcl` version 8.5 and
`loon` requires `Tcl` version 8.6. Hence, for the near future, you
need to install `Tcl` version 8.6 and link it with `R`. 

Download the **32 bit and 64 bit** builds of ActiveTcl
version 8.6.x. You need to install both.

* 64 bit build [download Windows (64-bit, x64) Tcl 8.6.x](http://www.activestate.com/activetcl/downloads/thank-you?dl=http://downloads.activestate.com/ActiveTcl/releases/8.6.4.1/ActiveTcl8.6.4.1.299124-win32-x86_64-threaded.exe)
* 32 bit build [download Windows (x86) Tcl 8.6.x](http://www.activestate.com/activetcl/downloads/thank-you?dl=http://downloads.activestate.com/ActiveTcl/releases/8.6.4.1/ActiveTcl8.6.4.1.299124-win32-ix86-threaded.exe)

Install the 64 bit and 32 bit ActiveTcl under `C:\Tcl64` and
`C:\Tcl32`, respectively . 

In the `C:/Tcl64/bin` and `C:/Tcl32/bin` folders copy paste the
`tk86.dll` and `tcl86.dll` files (create duplications) and rename them
to `tk85.dll` and `tcl85.dll`, respectively.

Create or modify the `.Rprofile` file in your home directory (usually
in `C:/Users/<your username>/.Rprofile` but you can check the home
path with `path.expand('~')` in `R`) to include the code below. To
create the `.Rprofile` use Rstudio and use the menu to create a new
Text File.

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
[youtube instructions](https://www.youtube.com/watch?v=2PsVBYNftrU). Note
that you need to install both 32bit and 64 bit Tcl.
