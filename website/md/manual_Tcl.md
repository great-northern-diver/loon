<script type="text/javascript">
document.getElementById("manual").className += " selected";
document.getElementById("manual_Tcl").className += " selected";
</script>

<link rel="stylesheet" href="style_tcl_man.css" type="text/css" />

### Name

`loon_plot` - creates a `loon` scatterplot widget

### Synopsis

~~~{.tcl}
loon_plot ?pathName? ?options?
~~~

### Widget Specific options

Options that apply to points can be specified as a vector or as a
single value. A single value will repeat itself for every point. The
term vector is used interchangeably with `tcl` list.

#### -active

a boolean vector specifying whether each point in `which`
should display itself or not.

#### -cex or -size

a numerical vector with the point sizes for the points in
`which`. Non-positive points display them self as filled squares.

#### -col or color

a vector specifying the color for the points in `which`. Valid colors
specifiers are either a textual
[Tk colorname](http://www.tcl.tk/man/tcl8.5/TkCmd/colors.htm) such as
`red`, 'green', or 'cornflower blue', or strings in the form `#RBG`,
`#RRBBGG`, `#RRRBBBGGG`, or `#RRRRBBBBGGG`, where each `R`, `B`, or
`G` represents a single hexadecimal digit.

#### -glyph

a vector of glyph ids for every point in `which`.

#### -linkingGroup

a string to set the linking group. If two loon plots
have the same linking group, then the `color`, `size`, `seleced`, and
`active` state of their points are synchronized between points with
the same linking tag. The linking tags are specified in the
`linkingKey` option.

#### -linkingKey

a vector of linking tags for the points in
`which`. The linking tags must be unique for each point within a plot.

#### -main or -title

title of the plot.

#### -panX

numeric value specifying the smallest x value shown in the
scatterplot.

#### -panY

numeric value specifying the smallest y value shown in the
scatterplot.

#### -selectBy

either "sweeping" or "brushing". Specifies the
interactive selection method.

#### -selected

a boolean vector specifying whether each point in
`which` should have a selected state.

#### -selectionLogic

one of "true", "false" or "not". "true" will add
newly brushed or swept points from the current selection. "false" will
remove newly brushed of swept points from the current selection. "Not"
will invert the selection state of the newly brushed or swept points.

#### -showScales

boolean value for showing the x and y scales or not.

#### -showLabels

boolean value for showing the axis labels or not.

#### -size or -col

see: `-col` or `-size`. 

#### -swapAxes

boolean value for swapping the x and y axis.

#### -sync

one of "push", "pull" and "ask". When changing the linking
group of a loon plot to one that has other plots associated to it one
must choose whether to "push" or "pull" the `selected`, `active`,
`color`, and `size` state of the points in the plot with the to or
from the newly linked plots.

#### -title or -main

see: `-main` or `-title`.

#### -transparent

boolean vector specifying whether the points in
`which` should display themself transparent. Except for image glyph
styles this means the glyph has a transparent fill color. Images
implement alpha blending.

#### -which

either one of "all", "selected", or "active", or a numeric vector of
point indices (starting from 0). `which` is used to specify which
points are addressed with the `selected`, `active`, `color`, `size`,
and `glyph` argument.

#### -x

coordinates

#### -xlab or -xlabel

x axis label.

#### -y

coordinates

#### -ylab or -ylabel

y axis label

#### -zoomX

zoom factor in x direction. `zoomX` of 1 will display make
the x axis range equal to the x data range. `zoomX` of 2 will make the
x axis range equal to half the x data range, etc...

#### -zoomY

zoom factor in y direction. `zoomY` of 1 will display make
the y ayis range equal to the y data range. `zoomY` of 2 will make the
y ayis range equal to half the y data range, etc...


### Introduction

The `loon_plot` command creates a new `loon` (scatter) plot widget. 

### Widget Commands

The `loon_plot` command creates a new `Tcl` command whose name is
*`pathName`*. The widget commands get called as follows

~~~{.tcl}
pathName command ?arg arg ...?
~~~

#### *pathName* **add** *what ?arg arg ...?*

`what` must be one of `glyph`, `drawing`, or `binding`. This command
will return the `id` of what has been added. The `id` is used to
`remove`, `reorder`, or `get` the `glyph`, `drawing`, or `binding`.

##### `glyph`: add point glyphs

New point glyphs must be specified for all points at once. The command
is as follows

~~~{.tcl}
pathName add glyph type name data
~~~

where `type` is one of `star`, `image`, and `text`. The `data` is a
list of length `n`, where `n` is the number of points, whose element
define the point glyph. The data list elements for the different types
are as follows

* `star` a list with `p` elements, where `p` is the number of radial
  axes, with values between `0` and `1`. Each value will be mapped
  onto a radial axis. `p` must be the same for each point.

* `image` image name

* `text` a string


##### `drawing`: add background drawings

Drawings are any canvas item type allowed by the
[`Tk canvas widget`](http://www.tcl.tk/man/tcl8.5/TkCmd/canvas.htm). The
coordinates of the drawings must be in the data coordinate system, not
in the canvas coordinate system.

The command is as follows

~~~{.tcl}
pathName add drawing type coords ?arg arg ...?
~~~

where the code after `drawing` can be exactly the same as behind
`canvasPathName create`. See the
[`canvas manual`](http://www.tcl.tk/man/tcl8.5/TkCmd/canvas.htm).


##### `binding`: specify binding for loon events {#loonBinding}

`loon` bindings differ from `Tk` bindings in that no event patterns
are accepted and the events are plot state related rather then `X`
event related.

~~~{.tcl}
pathName add binding event FUN
~~~

loon bindings provides two substitutions in `FUN`: `%W` and `%e` with

* `%W`: plot widget path name
* `%e`: list of events that generated the `FUN` evaluation

Valid events names are

| --- | --- | --- | --- |
| `all` | `pan` | `zoom` | `resize` |
| `selected` | `size` | `color` | `active` |
| `data` | `linkingGroup` | `linkingKey` | `brushOn` |
| `showScales` | `swapAxes` | `showLabels` | `labels` |

When adding a binding one can supply a list of events for the
`event` argument to have the code evaluated for any of the events in
the list.


#### *pathName* **cget** *option*

returns the current value or list of values of the configuration
option called *`option`*. *`option`* may be any of the values accepted
by the `loon_plot` command except `-which` and `-sync`.

#### *pathName* **configure** ?*option value*? ?*option value ...*?

Modify the configuration options of the plot widget. Multiple options
can be modified at once.

#### *pathName* **get** *what* ?*id*? ?*specific*?

Get information about *what* where *what* is one of `glyph`,
`drawing`, or `binding`. The command

~~~{.tcl}
pathName get what
~~~

will return a list of all `id`s for *what*. The command

~~~{.tcl}
pathName get what id
~~~

will return a list with all data about the item *what* with id
`id`. If a specific part of the data is needed then *specific* can be
submitted where the specifics for the different *what*'s are

* `glyph`: `type`, `name`, `data`
* `drawing`: `type`, `coords`, `args`
* `binding`: `event`, `FUN`


#### *pathName* **remove** *what* *id* ?*id* ...?

Remove one or more `glyph`, `drawing`, or `binding`. The first `id` can also be a list of `id`s.

#### *pathName* **reorder** *what* *id1* ... *idn*

Reorder the order of processing for `glyph`, `drawing`, or
`binding`. The `id`s may also be specified as a list. All `id`s for a
particular *what* must be listed.

#### *pathName* **replot**

Redraw the scatterplot. This is only important if there seems to be
inconsistencies on the scatterplot.



### Bindings

`loon` allows for two type of bindings: the standard `X` event
patterns bindings, and `loon` bindings for plot state events like
`zoom`, `pan`, `selected`, etc... For the latter see [dsa](#pathname-add-what-arg-arg-...)

A `loon` scatterplot display is built upon the `Tk` canvas
widget. Hence, for a `loon` plot canvas (say `can`), it is possible to
add `Tk` bindings with `bind $can ...` or `$can itembind ...`.

For the `bind $can ...` type of bindings it is important not to
overwrite bindings defined by us.

For the `$can itembind ...` of bindings it is important specify the
binding in terms of item tags and not item ids. The item ids change
constantly as `loon` redraws the screen.

We tag point glyphs as follows: Data points are tagged with `point1`,
`point2`, ..., `pointN`, where `N` is the number of points in the data
associated with the scatterplot. Every data point also has the tag
`data`, and points with size `<=0` also have a `zero` tag. Images have
the tag `image`, star glyphs the tag `star`, text glyphs the tag
`text`, and dots the tag `dot`.

To get the widget path name for the canvas run

~~~{.tcl}
pathName cget -canvas
~~~


