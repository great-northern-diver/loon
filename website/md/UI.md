<script>
document.getElementById("ui").className += " selected";
</script>

---
title: loon - user interface gestures
---

### Introduction

The keyboard and mouse gestures for the graphical user interfaces of
`loon`'s displays should be fairly easy to learn. We kept the number
of mouse gestures and keyboard shortcuts minimal. The `Ctrl` and
`Shift` in combination with the left and right mouse button or mouse
wheel is all you ever need. In fact, the default gestures for the
scatterplot display are summarized by the following figures.

Zoom and Pan gestures are as follows (the serialaxes display does not
support zoom and pan)

![Two superimposed mice illustrate a click and drag gesture](images/gestures_zoom_pan.png "Interaction Gestures: Zoom and Pan")

The selection gestures are as follows

![Two superimposed mice illustrate a click and drag gesture](images/gestures_select.png "Interaction Gestures: Selection")

And for the scatterplot display one can temporary move points as
follows

![Move points on scatterplot](images/gestures_move.png "Move points on scatterplot")


#### Macbook users

Note that on the Macbook the right button press and drag events can be
configured in *Settings* > *Trackpad* with the *Secondary click*
setting, see image below. For example, when set to "Click or tab with
two fingers" then a two-finger-press and motion event while keeping
the fingers pressed will generate the right-click dragging events
(e.g. for panning).

![](images/Macbook_Rightclick.png)


### User Interface Behavior

There are a few things which are good to know and are features rather
than bugs.


#### Inspector

* Only one `loon` inspector at a time can be open at a given time. If
  you would like to deactivate the `loon` inspector set the
  `useLoonInspector` state to `FALSE` (R-Code)

		p <- l_plot(iris, useLoonInspector=FALSE)
		pi <- l_plot_inspector(activewidget=p)


#### Resizing Points

* Points with `size` less equal `1` are shown with the same glyph
  size.
* Absolute resizing will use the minimum size of the selected points
  to determine the new size for all selected points.
* Relative resizing will add or subtract one size unit for each
  selected point.
* Decreasing the size of the selected points using the inspector will
  stop responding to button presses if any of the selected points is
  less equal than zero.

#### Layers

* Only the model layer is interactive!
* The button toolbar in the layers window
![Layers Toolbar](images/layers_buttons.png) has the functionality as
follows: ![up](images/icons/up.png) and ![down](images/icons/down.png)
move a layer or drawing one position up or down among the children of
its parent. The ![left](images/icons/left.png) moves the layer or
drawing to the grand parent layer and the
![right](images/icons/right.png) button moves the selected layer or
drawing into the layer directly below it if the item below the
selected element is a layer. ![visible](images/icons/visible.png) and
![invisible](images/icons/visible.png) turn the selected drawing or
layer and all its children visible and invisible,
respectively. ![plus](images/icons/plus.png) adds a new layer and
![minus](images/icons/minus.png) removes the selected element where it
is a layer it will delete all its children. Finally,
![scaleto](images/icons/scaleto.png) scales the plot such that the
selected layer is completely shown. 

### Modifier Keys

The default modifier keys used in `loon` are `Ctrl` and
`Shift`. Sometimes operating systems use some of these modifier keys
for system functions and hence wont pass their respective key events
to `Tk`. Also, different operating systems generate different events
for the mouse scroll wheel. If you experience problems with the above
mentioned gestures please try each gesture on the eventutil below and
[let me know](mailto:adrian@waddell.ch) which events do no get
generated on your system:

In `Tcl`

~~~
::loon::eventsutil
~~~

Or in `R`

~~~
.Tcl('::loon::eventsutil')
~~~


<!-- In Ubuntu you might have to
[change the default `Alt-drag` gesture behaviour](unity_disable_alt_drag.html). However
we were not successful so far. -->



