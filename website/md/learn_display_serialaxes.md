
<script>
window.onload = function() {
    document.getElementById("learn_display_serialaxes").className += " selected";
    setLearnUrl("display_serialaxes");
}
</script>

---
title: serialaxes display - loon
---

![](images/display_serialaxes_star.png "loon serialaxes display showing radial coordinates")

<R>
~~~
data(olive)
s <- l_serialaxes(data=olive, color=olive$Area, title="olive data")
~~~
</R>

<Tcl>
~~~
source olive.tcl
set s [serialaxes -data $olive -color $Area -title "olive data"]
~~~
</Tcl>

![](images/display_serialaxes_parallel.png "loon serialaxes display showing parallel coordinates")

<R>
~~~
s['axesLayout'] <- 'parallel'
~~~
</R>

<Tcl>
~~~
$s configure -axesLayout parallel
~~~
</Tcl>



# Serialaxes

* Get the state names with

<R>
		states <- l_info_states(s)
		names(states)
</R>

<Tcl>
	    set states [$s info states]
		dict keys $states
</Tcl>

* Query a state, say `sequence`, as follows

<R>
	    s['sequence']
</R>

<Tcl>
	    $s cget -sequence
</Tcl>

* Change a state, say again `sequence`, as follows

<R>
	    s['sequence'] <- names(olive)[c(2:10)]

	alternatively, and more efficient if you modify more than one
    state, use
	
		l_configure(s, sequence=names(olive)[c(2:10)])
</R>

<Tcl>
	    $s configure -sequence [lrange [dict keys $olive] 2 end]
</Tcl>

	![](images/display_serialaxes_parallel_nofactors.png "loon serialaxes display")
	


* When creating a plot you may specify any state at plot creation

<R>
	    s1 <- l_serialaxes(data=olive, color=olive$Area, title="olive data",
			sequence=names(olive)[2:10])
</R>

<Tcl>
	    set s1 [serialaxes -data $olive -color $Area -title "olive data"\
			-sequence [lrange [dict keys $olive] 2 end]]
</Tcl>


* details on a state, say `sequence`, is easily had with

<R>
	    states <- l_info_states(s)
		states$sequence

	and a particular field
	
		states$sequence$description
		
</R>

<Tcl>
	    set states [$s info states]
		dict get $states sequence

	and a particular field

	    dict get $states sequence description
</Tcl>




# Good To Know

* The serialaxes display does not support zooming and panning.
* Sweep selection is with a line and not with a rectangle.

# Data Scaling

The `scaling` state defines how the `data` is scaled. The axes display
0 at one end and 1 at the other. For the following explanation assume
that the `data` is in a `nxp` dimensional matrix. The `scaling`
options are then

* **variable:**  per column scaling.
* **observation:** per row scaling.
* **data:** whole matrix scaling.
* **none:** do not scale.

