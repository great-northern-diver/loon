


<script>
window.onload = function() {
    document.getElementById("learn_display_pairs").className += " selected";
    setLearnUrl("display_pairs");
}
</script>


---
title: scatterplot matrix - loon
---

![](images/display_pairs.png "loon scatterplot matrix")

<R>
~~~
ps <- l_pairs(data=iris, color=iris$Species)
~~~
</R>

<Tcl>
~~~ {.todo}
source iris.tcl
set ps [pairs -data $iris -color $Species]
~~~
</Tcl>

<div class='todo'>

* Note that currently the `data` argument does not refer to a state
  and, therefore, cannot be changed after creation.
* The scatterplot matrix is currently a compound widget. A single
  widget implementation is planned.

</div>





# Scatterplot Matrix

<R>

Note that in the above example `ps` is a list with the individual
scatterplot handles. Use

~~~
names(ps)
~~~

to figure out which plot handle is associated with which handle.

To reset the zoom and pan for each display to show all points you
could use `l_scaleto_plot` or `l_scaleto_world` for each plot on the
diagonal. However it is easier to just evaluate the function on all
plot handles:

~~~
sapply(ps, l_scaleto_plot)
~~~

</R>
