<script>
document.getElementById("home").className += " selected";
</script>


---
title: "loon - interactive data visualization"
---


# Introduction

`loon` is a toolkit for interactive data visualization and
exploration. Currently, `loon` is embedded in `R` and `Tcl`. Please
see the [beta section](beta.html) for the current state of the
software.

* To issue a bug report use the
[github issue tracker](https://github.com/waddella/loon/issues).

* You can fork `loon` [on github](https://github.com/waddella/loon). 

# Installation

* For `R` see the [beta section](beta.html).

* For `Tcl` see the [github Readme](https://github.com/waddella/loon/Tcl/).

# Statistical Displays

Currently we provide an interactive display for histograms,
scatterplots, serialaxes plots (i.e. star glyphs or parallel
coordinates) and navigation graphs.

# User Interface
	
The user interface of `loon` has two elements: the *display* and the
*inspector*.

![loon screenshot. Left: Inspector and Right: Scatterplot display.](images/loon_preview.png "loon screenshot with inspector and scatterplot display.")

The inspector lays out most of the functionality of the
display. Displays support further actions via mouse gestures and
modifier keys.


# Point Glyph Types

Points in scatterplot can be displayed with different glyphs. `loon`
distinguishes between primitive and non-primitive glyphs. The
primitive glyphs are available in all scatterplots whereas the
non-primitive ones need to be added to a scatterplot before they can
be used. [Read more.](learn_R_display_plot.html#glyphs)

![](images/point_glyph_types.png "loon point glyph types")


# Zoom and Pan

![](images/pan_zoom.png "Zoom and Pan")

When zooming and panning the plot region gets shown in relation to all
data on the worldview in the inspector.


# Linked Displays

![Selecting points in one display will also select them in all linked displays.](images/linking.jpg "Linking")

Multiple displays can be linked such that the linked points share
their color, size, selected and active states.

<div class="floatleft">
![](images/adhoc_linking.png "Adhoc Linking")
</div>

Scatterplot displays can be linked ad-hoc with the inspector by
changing the *Linking Group* of the scatterplot displays to be the
same. `loon` will always suggest a linking group that has no display
associated to it. The linking group `none` keeps the display unlinked.

Displays with different numbers of points can be linked too. The only
linking constraint is that a point can be linked with no more than one
point on another display.

# Selection

`loon` provides multiple interactive selection tools.
	
<div class="floatleft">
![](images/select_click.png "Click Select")
</div>

**Click Select:** individual points can be selected or deselected by
  simply clicking with the mouse cursor on them.

<div style="clear:both;margin-bottom:10px"></div> 

<div class="floatleft">
![](images/select_sweep.png "Sweep Select")
</div>

**Sweep Select:** a sweep selection selects all points below a
rectangular area which is defined by a left-click drag gesture. That
is, the upper left corner of the sweep rectangle is at the location of
the left-button press, and the lower right corner is at the current
cursor location while the left button is pressed.


<div style="clear:both;margin-bottom:10px"></div>

<div class="floatleft">

![](images/select_brush.png "Brush Select")
</div>
**Brush Select:** a brush selection also selects all points below a
rectangular area. However, in contrast to the sweep selection, the
rectangular brush area has a fixed size and a left-button press moves
the lower right corner to the current mouse location, and a left-click
dragging gesture moves the brush area along the mouse pointer.

<div style="clear:both;margin-bottom:10px"></div>

Additionally, we provide selection by point color and via the command
line.


# Modify

The state of the selected points can be modified with the tools and
actions in the *Modify* panel of the inspector. The following *Modify*
panel is from the scatterplot inspector.

<div class="floatleft" style="margin-right: 20px">
![](images/modify.png "Brush Select")
</div>


* Point color
* Active: whether a points are displayed or not. Reactivate will
  activate all points.
* Point position: points can be temporarily relocated on the
  scatterplot display. The tools in the *move* row align, distribute,
  grid-arrange, jitter and reset the temporary location of the
  selected points.
* Point glyph types
* Size: we provide relative and absolute resizing. Relative resizing
  will change the size of the selected points by +1 or -1. Absoute
  resizing will take the smalles point size in the selection and set
  all selected points to +1 or -1 of the smallest size.


<div style="clear:both;margin-bottom:10px"></div>


# Move Points

On the scatterplot display selected points can be temporarily moved to
a new location.

![](images/move.jpg "Interactively Move Points")


# Command Line Control

Displays can be completely controlled via the command line. The `R`
API is discussed in [learn R section](learn_R_intro.html). It is
similar to the `tcltk` package API. For example, in `R` for a plot
with handle `p` once can query a plot state as follows

~~~
# Query State
p['selected']
l_cget(p, 'selected')

# Modify State(s)
p['selected'] <- TRUE
l_configure(p, zoomX=2, zoomY=3, selected=FALSE)
~~~

The `Tcl` API follows the `Tk` widget convention and provides a `cget`
and `configure` widget function to query and modify plot states. The
following code demonstrates this where the variable `p` holds the
widget path name:

~~~
$p cget -x
$p configure -zoomX 2 -zoomY 3 -selected FALSE
~~~

# Event Bindings

`loon`'s event bindings provide the facility to have custom code
(i.e. callbacks) evaluated at specific events. We support a number of
different event types which, for example, include such changes in
point color, zoom, selection, moving the mouse cursor over a visual
element and window resizing.

~~~
$p bind state add {zoom pan} {
	puts "Zoomed or Panned"
}
~~~

or in `R`

~~~
l_bind_state(p, c('zoom', 'pan'), function() {cat('Zoomed or Panned')})
~~~

We provide various binding types. [Read more](learn_R_bind.html).

# Layers


The histogram, scatterplot and graph displays support layering of
lines, polygons, text, rectangles and more. For example, the following
picture shows a scatterplot of international airports with map
information layered underneath the scatterplot points. The data is
from the [naturalearth project](http://naturalearthdata.com/).

![](images/gallery/naturalearth_world.png)


Layers can be arranged with `loon`'s layers inspector.

![](images/layers.png "loon Layers")



</div>
