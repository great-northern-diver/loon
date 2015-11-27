
<script type="text/javascript">
window.onload = function() {
    document.getElementById("learn_Tcl_Tk").className += " selected";
    document.getElementById("learn_R").firstChild.href = "learn_R_tcltk.html";
    document.getElementById("learn_Tcl").firstChild.href = "learn_Tcl_Tk.html";
}
</script>


# Tk Concepts

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

A more detailed introduction to the `Tk` Concepts can be read on
[tkdocs.com](http://www.tkdocs.com/tutorial/concepts.html).

