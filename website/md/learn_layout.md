<script>
window.onload = function() {
    document.getElementById("learn_widgets").className += " selected";
    setLearnUrl("layout");
}
</script>


Rather then having a new window with a single scatterplot packed into
it, you can also specify a parent widget for the `Loon` plot widget
and place it using any layout manager you wish. To create the
scatterplot widget use the `parent` argument

~~~
tt <- tktoplevel()
p1 <- l_plot(parent=tt, x=c(1,2,3), y=c(3,2,1))
p2 <- l_plot(parent=tt, x=c(4,3,1), y=c(6,8,4))
~~~

You will only see an empty window and the scatterplots showing up in
the worldview of the inspector but the `loon` scatterplot widget will
not show up.  The `p1` and `p2` `Loon` plot widgets need to be placed
on the window using a geometry manager such as `pack` or `grid`. This
is how layouts of multiple `Loon` plot widgets in one window can be
achieved. For example, a 1 row 2 columns layout may be achieved with
the `grid` geometry manager as follows

~~~
tkgrid(p1, row=0, column=0, sticky="nesw")
tkgrid(p2, row=0, column=1, sticky="nesw")
tkgrid.columnconfigure(tt, 0, weight=1)
tkgrid.columnconfigure(tt, 1, weight=1)
tkgrid.rowconfigure(tt, 0, weight=1)
~~~

And if desired, you can specify a title for the window

~~~
tktitle(tt) <- "Loon plots with custom layout"
~~~
