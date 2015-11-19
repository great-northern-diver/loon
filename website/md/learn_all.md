<script>
document.getElementById("learn").className += " selected";
document.getElementById("learn_<R>R</R><Tcl>Tcl</Tcl><Python>Python</Python>").className += " selected";
</script>

### Introduction

<Tcl>
`loon` is written in `Tcl` and `Tk`. Users familiar with `Tk`
widgets will feel familiar with the `loon` API as the `loon` widgets
follow the standard `Tk` conventions.
</Tcl>


<R>
`loon` is available in `R` via the `loon` `R` package hosted on
`CRAN`.
</R>

The user interface is explained in the
[UI section](learn_UI.html). You will find there the explanations of
how to zoom, pan, select and modify points.

<R>
`loon` is written in `Tcl` and `Tk`. Users unfamiliar with the
language and toolkit may read <a href="learn_R_tcltk.html">our short
overview of R & tcltk</a>.
</R>

<Python>
`loon` is written in `Tcl` and `Tk`. Users unfamiliar with the
language and toolkit may read <a href="learn_Python_tcltk.html">our short
overview of Python & tcltk</a>.
</Python>



### Installation
<Tcl>
Get package from github

~~~
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
To install the newest stable version from CRAN run the following
command in an R session

~~~
install.packages("loon")
~~~

Development versions can be installed and tested from github with the
`devtools` `R` package as follows

~~~
library(devtools)

install_github("loon")

# use development version of loon
dev_mode(on=TRUE)

# switch back to stable version of loon
dev_mode(on=FALSE)
~~~
</R>



### Getting Started
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

You can now use `loon`. An example scatterplot is created with

~~~
loon_plot -x {4 1 5} -y {2 3 1} -color {red orange blue} -size {1.4 5.2 4}
~~~

If you want to use `loon` interactively via the `tcl` shell take a
look at the `tk` console [`tkcon`](http://tkcon.sourceforge.net/).
</Tcl>


<R>
Here a short session that might show all the `R` functions you will
ever need to work with `loon`

~~~
# Load the library
library("loon")

# Create a plot
p <- with(iris, loon_plot(x=Sepal.Width, y=Sepal.Length, color=Species))

# Modify the plot state
tkconfigure(p, size=iris$Petal.Length, color="blue")

# Query the plot state
tkcget(p, "-color")
~~~
</R>

The User Interface of the scatterplot display is explained in the
[UI section](learn_UI.html).




<R>
### R and the tcltk library

Although knowledge of `Tcl` and `Tk` is not needed to use `loon`, it
does help to have somw basic knowledge of `Tcl` and `Tk` to get a
better understanding of the programming interface we provide for
`loon` in `R`. `loon` is entirely programmed in `Tcl/Tk` and made
accessible from `R` with a very thin layer of `R` code.

`Tcl` is a programming language and `Tk` is a GUI toolkit written as
extension for `Tcl`. `Tk` has been bound to many other programming
languages such as `R`, `Python`, and `Perl` to name a few.

Please take to time to read
[**our short overview of the `tcltk` `R` package**](learn_R_tcltk.html).

If your knowledge of `R` is generally basic, then consider reading the
slides from the
[R introduction course](http://adrian.waddell.ch/RforEcon/) written by
Adrian Waddell.
</R>


<Tcl>

### Tk Concepts

For those new to `Tk`, it is useful to understand some concepts of
`Tk` as we loosely follow the standard `Tk` widget
behavior. Generally, [widgets](http://wiki.tcl.tk/490) are GUI
elements like buttons and sliders. The `loon` scatterplot display and
inspector can be considered as megawidgets; a widget that combines
several other widgets but behaves like a normal widget itself. Every
widget in a `Tk` session has a unique hierarchical window path name
with the root being a dot `.` and children and parents are separated
by a dot. Hence a typical widget path name for a widget `button` with
three ancestors (root, `frameA` and `frameB`) is
`.frameA.frameB.button` where the user is free to name all the nodes
(except the root) as he/she wishes. When a `Tk` widget gets created a
function with the function name equivalent to the widget path name
gets created to query or modify the widget. For example in `tkcon` run

~~~
package require Tk
button .b -text "Hello World"
pack .b
info commands
~~~

and you will see a button with the label "Hello World" and the output
for `info commands` will list the function `.b`.

The widget function can be used to query and modify the widget options
via `cget` and `configure`. For example continuing with the above
example

~~~
.b configure -text "A new label"
.b cget -text
~~~

A more detailed introduction about `Tk` Concepts can be read on
[tkdocs.com](http://www.tkdocs.com/tutorial/concepts.html).

</Tcl>


<R>

### loon Scatterplot

Try to use `loon_plot()` instead of `plot()`. We use mostly the same
syntax as is used with `R`'s base graphics engine:

~~~
p <- loon_plot(c(1,2,3), c(4,2,4), cex=c(4,1,8), col=c('red','blue','yellow'))
~~~

`x` and `y` are the only arguments that are allowed to be unnamed. All
other arguments must be named. `cex` and `col` are not the recommended
argument names as `size` and `color` appear more readable to us. Hence

~~~
p <- loon_plot(c(1,2,3), c(4,2,4), size=c(4,1,8), color=c('red','blue','yellow'))
~~~

creates exactly the same plot.

`loon_plot()` returns the widget path name which can then be used as a
handler to modify and query the state of the plot widget. The widget
path name is also shown in plot window title and has the form
`.number.1`. The windows path name can be manually assigned as a
character string to a variable which yields a valid `loon ` plot
handler:

~~~
p <- ".1.1"
~~~

Note that in `R` consecutive `plot()` calls write their output to the
same device (e.g. window), but consecutive `loon_plot()` calls will
open a new window for each call. If an existing `loon` plot should be
modified the `tkconfigure()` function with an `x` and/or `y` argument
should be used (see below).

</R>


### Modifying a Plot

The plot widget can be queried and modified with the <Tcl>`cget` and
`configure` subcommands</Tcl><R>`tkcget()` and `tkconfigure()`
functions</R>, respectively. This is standard `tk` widget behavior.

~~~{.tcl}
set p [loon_plot -x {1 2 3} -y {3 2 1} -color {red green orange}]

$p cget -color
$p cget -linkingGroup

$p configure -color {green green blue}
$p configure -size {2.1 3.2 4.10} -active {0 1 1} -selected {0 0 1}
~~~

~~~{.R}
p <- loon_plot(x=c(1,2,3), y=c(3,2,1), color=c("red","green","orange"))

tkcget(p, "-color")
tkcget(p, "-linkingGroup")

tkconfigure(p, color=c("green","green","blue"))
tkconfigure(p, size=c(2.1,3.2,4.10), active=c(FALSE,TRUE,TRUE),
	selected=c(0,0,1))
~~~

<R>Widget options in `tkconfigure()` must be specified in `key=value`
pairs. Whereas for `tkcget()` a preceding dash for the option is
necessary.</R>

Also note that per command execution only one state can be queried with
<Tcl>`cget`</Tcl><R>`tkcget()`</R> but multiple states can be modified
with <Tcl>`configure`</Tcl><R>'tkconfigure()'</R>.

A list of all states the can be queried or modified can be found in
the next section or will be returned with the following calls

~~~{.tcl .todo}
$p cget
$p configure
~~~

~~~{.R .todo}
tkcget(p)
tkconfigure(p)
~~~


### loon widget Options 

The widget path name of every `loon` scatterplot widget gets returned
by the <Tcl>`loon_plot` procedure</Tcl><R>`loon_plot()` function</R>
and can also be found in the scatterplot window title. You can assign
the path name to a variable, say `myplot`, in one of the following
ways

~~~{.tcl}
set myplot [loon_plot -x {1 2} -y {2 1}]
set myplot ".loon_1.plot"
~~~

~~~{.R}
myplot <- loon_plot(x=c(1,2), y=c(2,1))
myplot <- ".1.1"
~~~

You can then either use the variable or the widget path name to work
with the scatterplot widget, that is

~~~{.tcl}
$myplot cget -x
.loon_1.plot cget -x
~~~

~~~{.R}
tkcget(myplot, "-x")
tkcget(".1.1", "-x")
~~~

are equivalent.

The list of options to query and modify the state of a scatterplot
widget is

| --- | --- | --- | --- |
| `x` | `y` | `color` (`col`) | `size` (`cex`) |
| `selected` | `active` | `which` | `showArea` |
| `xlabel` (`xlab`) | `ylabel` (`ylab`) | `title` (`main`) |
| `zoomX` | `zoomY` | `panX` | `panY` |
| `linkingGroup` | `linkingKey` | `sync` | `glyph` |
| `showScales` | `showLabels` | `swapAxes` | `selectBy` |
| `selectionLogic` | `inspector` |

<Tcl>
Note that you need to add a dash when specifying them in `cget` or
`configure`, e.g. `-zoomX`.
</Tcl>

The state or value of each of the above options (except `which` and
`sync`) can be retrieved with the <Tcl>`cget` subcommand</Tcl><R>`tkcget()` function</R>.

A detailed description of each option can be found <Tcl>`loon` API
specifications</Tcl><R>in the `R` documentation</R>


~~~{.R}
help('loon_plot')
~~~


### Linking Points Between Multiple Plot Widgets

Multiple `loon` scatterplot displays can be linked such that their
points share selection, size, color and active state if the points
have the same *linking tag*. Every point in `loon` has one linking
tag, i.e. a string, which is by default the element index. The ordered
set of all linking tags for each point is called `linkingKey` in
`loon`. Points must have unique linking tags within a plot. This means
that a point can not be linked with another point in the same display
and it can share its linked state with at most one point per linked
scatterplot display.

Scatterplots, independent of their number of points, are linked if
they share the same `linkingGroup`.

![If scatterplots have the same `linkingGroup` then points with the same linking tag share their selection, size, color and active state. The linking tags are saved in the `linkingKey` option. ](images/linking.png "Linking Displays")

The code to create the above depicted situation is

~~~{.tcl}
loon_plot -x {0 0.1 1 2} -y {1 2 0.8 2}\
	-color {blue yellow orange green}\
	-size {3 3 5 4}\
	-selected {1 0 0 0}\
	-linkingKey {mike bob max rita}\
	-linkingGroup "Year 2002"
loon_plot -x {0 1 1.5 3 3.2 3.3} -y {0 5 4 2 5 3.8}\
	-color {green red blue green orange "medium turquoise"}\
	-size {2.5 3 3 4 5 3}\
	-selected {1 1 0 0 0 0}\
	-linkingKey {steve bob lea rita max lisa}\
	-linkingGroup "Year 2002"\
	-sync pull
~~~

<Tcl>where the backslash `\` can be used to split a `tcl` command onto
multiple lines.</Tcl>



~~~{.R}
loon_plot(x=c(0,0.1,1,2), y=c(1,2,0.8,2),
	color=c("blue","yellow","orange","green"),
	size=c(3,3,5,4),
	selected=c(TRUE,FALSE,FALSE,FALSE),
	linkingKey=c("mike","bob","max","rita"),
	linkingGroup="Year 2002")
loon_plot(x=c(0,1,1.5,3,3.2,3.3), y=c(0,5,4,2,5,3.8),
	color=c("green","red","blue","green","orange","medium turquoise"),
	size=c(2.5,3,3,4,5,3),
	selected=c(1,1,0,0,0,0),
	linkingKey=c("steve","bob","lea","rita","max","lisa"),
	linkingGroup="Year 2002",
	sync="pull")
~~~

The `sync` argument is optional and by default `ask`. If a plot
switches to a `linkingGroup` that has other plots linked to it, then
`T2kd` needs to know whether the new plot should `push` or `pull` its
state with the linked plots. Creating a new plot with the
`linkingGroup` argument is similar to switching the `linkingGroup`
from `none` to a new `linkingGroup`. For <Tcl>`-sync
ask`</Tcl><R>`sync="ask"`</R> `loon` will create a small GUI dialog
asking whether to `push` or `pull` the plot state.


The *linkingGroup* pull-down menu on the inspector will always suggest
a `linkingGroup` that has no other display associated to (indicated
with `(0)` after the `linkingGroup`). Note that the inspector always
adds a space and the number of displays having the same `linkingGroup`
in the pull-down menu. The space and number in brackets are not part
of the `linkingGroup`.


The default `linkingGroup` for a scatterplot is `none`, a keyword
indicating that the scatterplot should not be linked.


### Glyphs, Drawings and Bindings

The API to add glyphs, background drawings (images, polygons, lines,
etc.) and hooks does not use the `cget` and `configure` widget
subcommands. Instead, we provide the <Tcl>widget
subcommands</Tcl><R><functionality> `add`, `remove`, `get`, and
`reorder`.

The general command structure is as follows:

~~~{.tcl}
widget action what "further arguments"
~~~

~~~{.R}
loon_action.what(widget, arguments)
~~~

Every hook, drawing and glyph gets an unique `id`. The `add` action
always returns an `id` for whatever was just added.

The `what` is always singular even though the action can be carried
out on several items using one call.



#### Glyphs

Glyphs are graphical representations data points. `loon` currently
allows the point glyph types: dots, images, text and star glyphs
(radial axes). In `loon` new glyphs can only added if they are
specified for all points at once. We show the functionality of glyphs
with a sample plot of `3` points. We first create and draw three
different images

~~~{.tcl}
set img1 [image create photo -width 30 -height 20]
$img1 put blue -to 0 0 30 20; $img1 put yellow -to 0 6 30 11
set img2 [image create photo -width 80 -height 80]
$img2 put red -to 0 0 80 80; $img2 put green -to 30 30 60 60
set img3 [image create photo -width 60 -height 100]
$img3 put orange -to 0 0 60 100; $img3 put black -to 40 0 50 80
~~~

~~~{.R}
img1 <- tkimage.create('photo', width=30, height=20)
tcl(img1, 'put', 'blue', '-to', 0, 0, 30, 20)
tcl(img1, 'put', 'yellow', '-to', 0, 6, 30, 11)
img2 <- tkimage.create('photo', width=80, height=80)
tcl(img2, 'put', 'red', '-to', 0, 0, 80, 80)
tcl(img2, 'put', 'green', '-to', 30, 30, 60, 60)
img3 <- tkimage.create('photo', width=60, height=100)
tcl(img3, 'put', 'orange', '-to', 0, 0, 60, 100)
tcl(img3, 'put', 'black', '-to', 40, 0, 50, 80)
~~~

<Tcl>where the semicolon `;` separates two `tcl` commands on the same
line.</Tcl>

Note that the images above have different sizes. `loon` will resize
the images (all glyph types) such that their areas are proportional to
their associated point sizes.

The different glyph types are then added as follows

~~~{.tcl}
set p [loon_plot -x {1 2 3} -y {3 1 2}\
	-color {green yellow blue} -size {4 3 7}]

$p add glyph stars "My Name"\
	[list [list 0.1 0.4 0.3]\
	      [list 0.5 0.7 0.9]\
		  [list 0.6 1 0]]
$p add glyph text "Other Label" {A B C}
$p add glyph images "Abstract Art" [list $img1 $img2 $img3]
~~~

~~~{.R}
p <- loon_plot(x=c(1,2,3), y=c(3,1,2),
	color=c('green','yellow','blue'), size=c(4,3,7))

loon_add.glyph(p, 'stars', 'My Name',
	loon_stars(data=data.frame(A=c(7,2,5),B=c(3,7,8),C=c(6,4,1)),
		sequence=c("A","B","C","A","C","B")))
loon_add.glyph(p, 'text', "Other Label", c("A","B","C"))
loon_add.glyph(p, 'images', "Abstract Art", c(img1,img2,img3))
~~~

The glyph names "My Name", "Other Label" and "Abstract Art" are used
in the pull down selector on the inspector to choose what glyphs the
selected points should be displayed with.


The <Tcl>`get` subcommand</Tcl><R>`loon_get.glyph()` function</R> will
either return a list of all glyph ids.

~~~{.tcl}
$p get glyph
~~~

~~~{.R}
loon_get.glyph(p)
~~~

Or to return the glyph data itself specify the glyph id

~~~{.tcl}
$p get glyph glyph1
~~~

~~~{.R}
loon_get.glyph(p,"glyph1")
~~~

The returned value is a list with type, name, and data. Individual
elements can directly be obtained obtained using

~~~{.tcl}
$p get glyph glyph1 type
$p get glyph glyph1 name
$p get glyph glyph1 data
~~~

~~~{.R}
loon_get.glyph(p, "glyph1", "type")
loon_get.glyph(p, "glyph1", "name")
loon_get.glyph(p, "glyph1", "data")
~~~

Glyphs can be deleted with the `remove` glyph command.

~~~{.tcl}
$p remove glyph glyph1
~~~

~~~{.R}
loon_remove.glyph(p, "glyph1")
~~~

The order of the glyphs is only used in the inspector pull-down
menu. The order can be changed with the `reorder` subcommand.

~~~{.tcl}
$p reorder glyph {glyph2 glyph0}
~~~

~~~{.R}
loon_reorder.glyph(p, c("glyph2","glyph0"))
~~~

The glyphs can be assigned either by first selecting points and then
choosing the glyphs via the inspector or also with a command line call

~~~{.tcl}
$p configure -glyph {glyph0 glyph2 dot}
~~~

~~~{.R}
tkconfigure(p, glyph=c("glyph0", "glyph2", "dot"))
~~~

where `dot` is an accepted keyword.


<R>

##### Images in Tcl

Images must be in the standard [`image`][tclman_image] format of
`tcl`. Hence, you may read the [`image` manual][tclman_image] to get a
deeper understanding of creating, deleting, and querying images in
`tcl`.

We do, however, provide two functions that simplify importing images
from a set of files (jpg, png, bmp, etc.), and from an array with
greyscale image information in its rows or columns.

If the images are stored in files in `jpg`, `png`, or `bmp` format you
can use our `loon_images_import_files()` `R` function. The core `tcl`
interpreter does not know many image formats, but with `tcl`
[`Img` package](http://wiki.tcl.tk/1404) loaded you can import images
in the most common formats.

~~~
paths <- dir('path-to-folder-with-images', '*.png', full.names=TRUE)
imgs <- loon_images_import_files(paths)
~~~

The `tcl` interpreter handles the image data internally and makes the
image objects accessible via handles named `image1`, `image2` and so
on. The `imgs` object from above is hence a vector with the image
object names. You can see the images in our image viewer as follows

~~~
loon_imageViewer(imgs)
~~~

To add the images to a plot widget

~~~
loon_glyphs.add(p, 'images', "faces", imgs)
~~~

It is possible to
[fill pictures pixel by pixel or row by row](http://wiki.tcl.tk/643)
as follows

~~~
library(tcltk); tt <- tktoplevel(); can <- tkcanvas(tt); tkpack(can)
pic <- tkimage.create('photo', width=20, height=30)
tkcreate(can, 'image', 50, 50, image=pic)
tcl(pic, 'put', 'yellow', '-to', 0, 0, 20, 30) # fill complete image
tcl(pic, 'put', 'red', '-to', 10, 15) # one pixel
# and row by row
.Tcl(paste(pic, 'put [list [lrepeat 16 blue] [lrepeat 16 green]] -to 0 17'))
~~~


##### Star Glyphs

The data structure in `tcl` to represent a set of star glyphs is a
list of lists where each nested list contains the coordinates for a
star glyph scaled to lie between 0 and 1.

The `loon` `R` package provides the `loon_stars()` function to work
with data frames. It takes a data frame with only numerical variables
and the variable sequence as its argument. The variable sequence may
either be composed of variable names or variable indexes.

~~~
stars1 <- loon_stars(data=iris, sequence=names(iris[,-5]))
library(PairViz) # for Hamiltonian decompositions
stars2 <- loon_stars(data=iris, sequence=hpaths(n=4,matrix=FALSE))
~~~

If the data is already scaled you can set the `scale=FALSE`
argument. The data frame must contain all numeric variables as the
scaling is performed on the complete `data` argument.

To add the star glyphs to a plot run

~~~
p <- with(iris, loon_plot(x=Sepal.Length, y=Petal.Length, color=Species))
loon_add.glyph(p, 'stars', "All Vars", stars1)
loon_add.glyph(p, 'stars', "Hamiltonian Path", stars2)
~~~

</R>

#### Drawings and Layers

Drawings are
[canvas items](http://www.tcl.tk/man/tcl8.4/TkCmd/canvas.htm#M95)
(i.e. arc, bitmap, image, line, oval, polygon, rectangle) that can be
placed on a `loon` scatterplot behind the data points. Drawings do not
respond to selection and modification and drawing items are not part
of the plot state accessible via <Tcl>`cget` and
`configure`</Tcl><R>`tkcget()` and `tkconfigure()`</R>.

The drawings must be specified in data coordinates and all valid
canvas item arguments including `tag` may be used. Hence the commands
to create drawings read almost the same as the ones used for drawing
on a `tk` canvas.

Two additional arguments when adding a drawing are the `label` and
`layers` argument. Drawings can be grouped within layers. Layers may
be nested. The <Tcl>`loon_layers` command</Tcl><R>loon_layers()
function</R> creates a tree view of the layers and drawings. The
layers widget allows to toggle the visibility of individual drawings
or layers. The labels of drawings define how the drawings are labeled
in the tree view. Note that the treeview always shows an item labeled
`points` to indicate the scatterplot glyphs rendering order. The
layers and drawings that appear first in the tree view widget get
rendered last and hence appear on top.

Because the drawings are specified in data coordinates they scale when
zooming in and out. The `width` argument wont scale and hence lines
and outlines have always the same width independent of zoom state.

Adding lots of drawings may slow down zooming and panning on the
scatterplot display.

Adding spaces into layer names is not supported!

As with glyphs and hooks, the subcommands `add`, `get`, `remove` and
`reorder` can be used to work with drawings.

~~~{.tcl}
set p [loon_plot -x {1 20 30} -y {30 10 20}\
	-color {green yellow blue} -size {4 3 7}]

loon_layers

$p add drawing oval 0 5 25 15 -fill yellow
$p add drawing polygon 10 15 20 40 21 8\
	-activefill "orange" -label "Orange Polygon"
$p add drawing text 27 28 -text "Hello\nWorld"\
	-font "Arial 30" -fill "red" -label "Text" -layer "Layer_1"
$p add drawing rectangle 2 19 20 15\
	-fill gray -outline green -width 8\
	-layer {"Layer_1" "another_layer"}
$p add drawing arc 25 18 35 8\
	-start 200 -extent 250 -style pieslice -fill ""\
	-label "Pie Slice" -layer {AA BB CC}
$p add drawing line 0 10 15 20 25 15 40 30\
	-width 5 -dash {8 4} -fill orange\
	-label "Line" -layer {AA BB}

set img [image create photo -width 30 -height 20]
$img put blue -to 0 0 30 20; $img put yellow -to 0 6 30 11
	
$p add drawing image 8 25 -image $img\
-layer {AA CC}
~~~


~~~{.R}
p <- loon_plot(x=c(1,20,30), y=c(30,10,20),
	color=c('green','yellow','blue'), size=c(4,3,7))

loon_layers()

loon_add.drawing(p, 'oval', c(0,5,25,15), fill="yellow")
loon_add.drawing(p, 'polygon', c(10,15,20,40,21,8),
	activefill="orange", label="Orange Polygon")
loon_add.drawing(p, 'text', 27, 28, text="Hello\nWorld",
	font="Arial 30", fill="red", label="Text", layer="Layer_1")
loon_add.drawing(p, 'rectangle', c(2,19,20,15),
	fill="gray", outline="green", width=8,
	layer=c("Layer_1","another_layer"))
loon_add.drawing(p, 'arc', c(25, 18, 35, 8), start=200,
	extent=250, style="pieslice", fill="",
		label="Pie Slice", layer=c("AA", "BB", "CC"))
loon_add.drawing(p, 'line', c(0,10,15,20,25,15,40,30),
	width=5, dash=c(8,4), fill="orange",
	label="Line", layer=c("AA","BB"))

img <- tkimage.create('photo', width=30, height=20)
tcl(img, 'put', 'blue', '-to', 0, 0, 30, 20)
tcl(img, 'put', 'yellow', '-to', 0, 6, 30, 11)

loon_add.drawing(p, 'image', c(8, 25), image=img,
	layer=c("AA","CC"))
~~~

<R>

The coordinates must not necessarily be specified in a vector

~~~{.R}
loon_add.drawing(p, 'polygon', 10, 15, 20, 40, 21, 8, fill="red")
~~~

works as well. If the coordinates are stored in an `x` and `y` vector, then you may use the `loon_coords()` function

~~~{.R}
xcoords <- c(10,20,21)
ycoords <- c(15,40,8)
loon_add.drawing(p, 'polygon', loon_coords(xcoords,ycoords), fill="green")
~~~

</R>

See the
[`canvas` manual](http://www.tcl.tk/man/tcl8.6/TkCmd/canvas.htm#M124)
for all the available drawing/item types and their options.

To list all the drawing `id`s their layer hierarchy run

~~~{.tcl}
$p get drawing
~~~

~~~{.R}
loon_get.drawing(p)
~~~

which returns a list with nested lists for each non-empty layer. Empty
layers are omitted. For the above example the returned list looks as
follows:

~~~{.tcl}
{{AA {BB d5 {CC d6 d4}}} {Layer_1 {another_layer d3} d2} d1 d0 points}
~~~
<Tcl>
Note that only the "root layer" is not named.
</Tcl>

~~~{.R}
list( "AA"=list("BB"=list("d5" , "CC"=list("d6", "d4"))),
	"Layer_1"=list("another_layer"=list("d3"), "d2"), "d1", "d0", "points")
~~~

If the layers hierarchy is not of interest use the `flat` argument

~~~{.Tcl}
$p get drawing -flat
~~~

~~~{.R}
loon_get.drawing(p, flat=TRUE)
~~~

To get the drawing data for a particular drawing `id` run

~~~{.tcl}
$p get drawing drawing3
~~~

~~~{.R}
loon_get.drawing(p, "d3")
~~~

The returned value is a list with type, coordinate list, and
arguments. Individual parts can be extracted using

~~~{.tcl}
$p get drawing d3 type
$p get drawing d3 coords
$p get drawing d3 args
~~~

~~~{.R}
loon_get.drawing(p, "d3", "type")
loon_get.drawing(p, "d3", "coords")
loon_get.drawing(p, "d3", "args")
~~~

The order of the drawings determines the order of how the drawings get
drawn. Drawings are, however, always behind the points. To reorder the
drawings using no layers use the `reorder`
<Tcl>subcommand</Tcl><R>function</R> as follows

~~~{.tcl}
$p reorder drawing {points d4 d3 d6\
	d0 d5 d1 d2}
~~~

~~~{.R}
loon_reorder.drawing(p, c('points','d4','d3','d6',
	'd0','d5','d1','d2'))
~~~

If the drawings should be ordered in layers use the nested list
structure as shown above with the <Tcl>`$p cget
drawing`</Tcl><R>loon_cget.drawing(p)</R> return value. (Remember not
to add spaces in layer name)

~~~{.tcl}
$p reorder drawing {points {layer_1 d1 d2\
	{layer_2 d4} d0} d3 {layer_3 d5 d6}}
~~~

~~~{.R}
loon_reorder.drawing(p,list("points", layer_1=list("d1","d2",
	layer_2=list("d4"), "d0"), "d3", layer_3=list("d5","d6")))
~~~



The drawings can also be removed from a scatterplot widget. <Tcl>It
does not matter whether the drawing ids are in a list or not.</Tcl>

~~~{.tcl}
$p remove drawing d0 d1
$p remove drawing {d2 d3}
~~~

~~~{.R}
loon_remove.drawing(p, c('d2','d0'))
~~~


#### loon Bindings

`loon` bindings arrange for plot state changes to invoke `Tcl`
scripts. `loon` bindings are similar to
[`Tk` bindings](http://www.tcl.tk/man/tcl8.6/TkCmd/bind.htm) except
that `Tk` bindings bind to event patterns (from X events) and `loon`
bindings bind to single events (from a plot state changes like `zoom`
and `color`).

The syntax for `loon` bindings differs considerably from `Tk`
bindings, as we use the same `add`, `get`, `remove` and `reorder`
framework as is used for drawings and glyphs. 

<Tcl>

`loon` bindings have two
[script substitutions](http://www.tcl.tk/man/tcl8.6/TkCmd/bind.htm#M24)

* `%W` for plot widget path name
* `%e` for a list of the events that triggered the function call

</Tcl>



The valid events are a subset of the configure options (no `sync` and
`which`):

| --- | --- | --- | --- |
| `x` | `y` | `color` (`col`) | `size` (`cex`) |
| `selected` | `active` | `which` | `showArea` |
| `xlabel` (`xlab`) | `ylabel` (`ylab`) | `title` (`main`) |
| `zoomX` | `zoomY` | `panX` | `panY` |
| `linkingGroup` | `linkingKey` | `glyph` |
| `showScales` | `showLabels` | `swapAxes` | `selectBy` |
| `selectionLogic` | `inspector` |

For example, if the <Tcl>procedure `foo`</Tcl><R>`foo` function</R>
should be evaluated every time a `resize` event occurs (the user
resizes the plot) then the particular binding is added as follows

~~~{.tcl}
set p [loon_plot -x {1 20 30} -y {30 10 20}]

proc foo {widget events} {
	puts "Widget $widget had the events: $events"
}

$p add binding {zoomX zoomY} {foo %W %e}
~~~


~~~{.R}
p <- loon_plot(x=c(1,20,30), y=c(30,10,20))

foo <- function(W, e) {
	cat(paste("Widget", W, "had the events:",
		paste(e, collapse=" "), "\n"))
}

loon_add.binding(p, c("zoomX", "zoomY"), foo)
~~~


<R> The arguments of the `foo` function must exist and be named `W`
and `e`, where 

* `W` is plot widget path name
* `e` is a list of the events that triggered the function call

Also, it is important tho note that the link between the `foo()` `R`
function and the `Tcl` interpreter is the `Tcl` callback

~~~{.R}
.Tcl.callback(foo) 
~~~

which will return something like `R_call 0x19d3440 %W %e`. The
hexadecimal number is hex-encoded address of the `foo` function. If
`foo` gets redefined with

~~~{.R}
foo <- function(W,e) {
	cat("Another foo\n")
}
~~~

the `foo` variable will point to a new function with hex-encoded
address

~~~{.R}
.Tcl.callback(foo) 
~~~

and the function at address `0x19d3440` will eventually be garbage
collected and the binding will throw an error.


If the binding function gets redefined many times it might be better
to work with two functions, one calling `foo`

~~~{.R}
bar <- function(W,e) {
	foo(W,e)
}
foo <- function(W,e) {
	cat("Yet another foo.\n")
}

loon_add.binding(p, "zoomX", bar)
~~~

or more direct with an

~~~{.R}
loon_add.binding(p, "zoomX", function(W,e){foo(W,e)})
~~~


~~~{.todo}
This why does this not get garbage collected?
Or will the previous function not get garbage
collected once tcl has a callback?
~~~
</R>

It is possible to bind to multiple events at once by specifying the
events in a list

~~~{.tcl}
$p add binding [list color size selected] {
	puts "new event from plot widget %W"
	if {"selected" in %e} {
		set sel [%W cget -selected]
		set sum 0
	    foreach s $sel {
			if {$s} {
		        incr sum
	        }
	    }
		puts "now $sum points are selected."
	}
}
~~~

~~~{.R .todo}
loon_add.binding(c("color", "size", "selected"),
	function(W,e) {
		if("selected" %in% e) {
			sel <- loon_cget(p, "-selected")
			cat(paste("now", sum(sel), "are selected"))
		}
	})
~~~

<Tcl>

Although it is possible to put all functionality/code into the binding
code block, it has speed advantages to wrap the code into procedures
and call those in the event bindings.

</Tcl>

All binding ids are listed by using the <Tcl>`get` widget
subcommand</Tcl><R>`loon_get.binding()` function</R>

~~~{.tcl}
$p get binding
~~~

~~~{.R}
loon_get.binding(p)
~~~

and the bindings information can be retrieved using the binding id

~~~{.tcl}
$p get binding binding1
~~~

~~~{.R}
loon_get.binding(p, "binding1")
~~~

The returned value is a list with the events and the code block to be
evaluated for the particular binding. If only the events or only the
code block should be returned run

~~~{.tcl}
$p get binding binding1 event
$p get binding binding1 FUN
~~~

~~~{.R}
loon_get.binding(p, "binding1", "event")
loon_get.binding(p, "binding1", "FUN")
~~~

Bindings are evaluated in the order they are listed in <Tcl>`$p get
binding`</Tcl><R>`loon_get.binding(p)`</R>. To reorder the evaluation
sequence use

~~~{.tcl}
$p reorder binding binding1 binding0
~~~

~~~{.R}
loon_reorder.binding(p, c("binding1", "binding0"))
~~~

And to remove a binding use 

~~~{.tcl}
$p remove binding all 
~~~

~~~{.R}
loon_remove.binding(p, "all")
~~~


#### Tk Bindings


["`loon` bindings arrange for plot state changes to invoke `Tcl` scripts."](http://www.tcl.tk/man/tcl8.6/TkCmd/bind.htm). For
example a button press, mouse scroll, or a key press on a widget can
be bound to evaluate some user defined code. `Tk` bindings are native
to `Tk` and are very flexible and powerful. `loon` builds on top of
`Tk` widgets and all interactivity is implemented using `Tk`
bindings.

There are two types of bindings for the canvas widget (say with widget
path name save in the `can` variable). First, `bind $can ...` binds
towards `X` event patterns on the whole canvas widget. Second, `$can
itembind ...` binds towards `X` event patterns on the canvas items.

For the `bind $can ...` type of bindings it is important not to
overwrite bindings defined by us.

For the `$can itembind ...` of bindings it is important specify the
binding in terms of item tags and not item ids. The item ids change
constantly as `loon` redraws the screen.

Tags added to the canvas items by `loon` are as follows: Data points
are tagged with `point1`, `point2`, ..., `pointN`, where `N` is the
number of points in the data associated with the scatterplot. Every
data point also has the tag `data`, and points with size `<=0` also
have a `zero` tag. Images have the tag `image`, star glyphs the tag
`star`, text glyphs the tag `text`, and dots the tag `dot`.

To get the canvas widget path name run

~~~{.tcl}
set p [loon_plot -x {1 2} -y {2 3}]

set can [$p cget -canvas]
~~~


~~~{.R}
p <- loon_plot(x=c(1,2), y=c(2,3))

can <- tkcget(p, "-canvas")
~~~

The, for example, to print `Mouse over Point 1` once the mouse pointer
enters the area of point one run

~~~{.tcl}
$can bind point1 <Enter> {puts "Mouse over Point 1"}
~~~

~~~{.R}
tkitembind(can, "point1", "<Enter>", function(...){
	cat("Mouse over Point 1\n")
})
~~~

You can also define bindings for drawings, but it is important to bind
towards a tag and not the item id as `loon` frequently redraws the
scatterplot from scratch and hence canvas item ids are generally not
useful as they have a very short live time.


Also note that you can use logic with the tag, for example

~~~{.tcl}
$can bind "point1||point2" <Button-1> {puts "pushed on point 1 or 2"}
~~~

~~~{.R}
tkitembind(can, "point1||point2", "<Button-1>", function(...) {
	cat("pushed on point 1 or 2\n")
})
~~~

To inspect the code for a particular binding run

~~~{.tcl}
$can bind "point1||point2" <Button-1>
~~~

~~~{.R}
tkitembind(can, "point1||point2", "<Button-1>")
~~~


To add code to a binding run

~~~{.tcl}
$can bind "point1||point2" <Button-1> {+ puts "added code"}
~~~

~~~{.R}
tkitembind(can, "point1||point2", "<Button-1>",
	paste("+", .Tcl.callback(function(...){cat("added code\n")})))
~~~

To delete a binding run

~~~{.tcl}
$can bind "point1||point2" <Button-1> {}
~~~

~~~{.R}
tkitembind(can, "point1||point2", "<Button-1>", "")
~~~

Note that adding and deleting `Tk` bindings is fundamentally different
than adding and deleting `loon` bindings. Also `loon` bindings do not
allow to bind towards event patterns and tag logic.



<R>

### Maps

The `loon` `R` packages comes with a `loon_add.map()` function to plot
`map` objects from the `maps` `R` package. The functions draws the
maps in the `maps` library with polygons. To plot the maps from the
`maps` library, install and load the `maps` package with

~~~{.R}
install.packages("maps", dependencies=TRUE)
library(maps)
~~~

The maps can be added as follows

~~~{.R}
canada <-  map("world", "Canada", plot=FALSE, fill=TRUE)
p <- loon_plot()
mapdrawings <- loon_add.map(p, map=canada, fill="", outline="black", width=3)
~~~


To add points use

~~~{.R}
data("world.cities", package = "maps")
canada.cities <- subset(world.cities,
	grepl("canada", country.etc , ignore.case=TRUE))

tkconfigure(p, x=canada.cities$long, y=canada.cities$lat)

cityGlyphs <- loon_add.glyph(p, 'text', "Cities", canada.cities$name)
~~~

~~~{.R .todo}
tkconfigure(p, glyph=tclvalue(cityGlyphs))
~~~

To delete all maps you must delete their drawings

~~~{.R}
loon_remove.drawing(p, mapdrawings)
~~~

We also provide some helper functions to deal with map objects from
the `maps` library, i.e. the `exploreMap` and `plot.map`
functions. Say you are looking for the polygon to plot France.

~~~{.R}
world <- map("world",  ".", fill=TRUE, plot=FALSE)
exploreMap(world)
~~~

And you will find that the correct name for the polygon drawing France
is called "France". You can then plot the outline of France with the
default R graphics engine

~~~{.R}
france <- map("world",  "^France$", fill=TRUE, plot=FALSE)
plot(france)
~~~

or plot it with `loon`

~~~{.R}
p <- loon_plot()
loon_add.map(p, france)
~~~


</R>


### Layout of Multiple Plot Widgets

Rather then having a new window with a single scatterplot packed into
it, you can also specify a parent widget for the `loon` plot widget
and place it using any layout manager you wish. To create the
scatterplot widget use the `loon_plot_widget()` function

~~~{.R}
tt <- tktoplevel()
p1 <- loon_plot_widget(tt, x=c(1,2,3), y=c(3,2,1))
p2 <- loon_plot_widget(tt, x=c(4,3,1), y=c(6,8,4))
~~~

You will only see an empty window and the scatterplots showing up in
the worldview of the inspector but the `loon` scatterplot widget will
not show up.  The `p1` and `p2` `loon` plot widgets need to be placed
on the window using a geometry manager such as `pack` or `grid`. This
is how layouts of multiple `loon` plot widgets in one window can be
achieved. For example, a 1 row 2 columns layout may be achieved with
the `grid` geometry manager as follows

~~~{.R}
tkgrid(p1, row=0, column=0, sticky="nesw")
tkgrid(p2, row=0, column=1, sticky="nesw")
tkgrid.columnconfigure(tt, 0, weight=1)
tkgrid.columnconfigure(tt, 1, weight=1)
tkgrid.rowconfigure(tt, 0, weight=1)
~~~

And if desired, you can specify a title for the window

~~~{.R}
tktitle(tt) <- "loon plots with custom layout"
~~~


### Export plot to an image

<Tcl>

`loon` plots may be exported by exporting the underlying `Tk` canvas
as a postscript using the `postscript` `canvas` widget command as
follows

~~~{.tcl}
set p [loon_plot -x {1 2 3} -y {4 1 2}]

set can [loon_plot cget -canvas]

$can postscript -file {~/loonplot.ps}
~~~


</Tcl>

<R>

`loon` plots may be exported as postscript files using the
`postscript` widget command of the `Tk` `canvas` widget. This is
possible as a `loon` plot is built on the `Tk` `canvas` widget.

~~~{.R}
p <- loon_plot(x=c(1,2,3), y=c(4,1,2))

loon_postscript(p, file="~/loonplot.ps")
~~~

</R>


[tclman_image]: https://www.tcl.tk/man/tcl/TkCmd/image.htm "Tcl image manual"
