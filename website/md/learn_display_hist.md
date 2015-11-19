<script>
window.onload = function() {
    document.getElementById("learn_display_hist").className += " selected";
    setLearnUrl("display_hist");
}
</script>

![](images/display_histogram.png "loon histogram")

<R>
~~~
data(olive)
h <- l_hist(x=olive$oleic)
~~~
</R>

<Tcl>
~~~
source olive.tcl
set h [histogram -x $oleic -xlabel oleic]
~~~
</Tcl>

# Histogram


* Get the state names with

<R>
		states <- l_info_states(h)
		names(states)
</R>

<Tcl>
	    set states [$h info states]
		dict keys $states
</Tcl>

* Query a state, say `showBinHandle`, as follows

<R>
	    h['showBinHandle']
</R>

<Tcl>
	    $h cget -showBinHandle
</Tcl>

* Change a state, say again `showBinHandle`, as follows

<R>
	    h['showBinHandle'] <- FALSE

	alternatively, especially if you need to modify more than one
    state, use
	
		l_configure(h, showBinHandle=FALSE)
</R>

<Tcl>
	    $h configure -showBinHandle FALSE
</Tcl>

* When creating a plot you may specify any state at plot creation

<R>
	    h1 <- l_hist(x=olive$oleic, showBinHandle=FALSE, yshows='density',
			color=olive$Area, showStackedColors=TRUE)
</R>

<Tcl>
	    set h1 [histogram -x $oleic, -showBinHandle FALSE\
			-yshows density -color $Area -showStackedColors TRUE]
</Tcl>


* details on a state, say `showBinHandle`, is easily had with

<R>
	    states <- l_info_states(h)
		states$showBindHandle

	and a particular field
	
		states$showBindHandle$description
		
</R>

<Tcl>
	    set states [$h info states]
		dict get $states showBindHandle

	and a particular field

	    dict get $states showBinHandle description
</Tcl>


# Good To Know

* When changing `yshows` to `frequency` or `density` per command line,
  you probably have to re-scale the plot as the y ranges of the
  histogram visualization changes
<R>
	    l_scaleto_world(h)
</R>
<Tcl>
	    $h scaleto world 
</Tcl>
   When changing `yshows` via the inspector the rescaling happens automatically.

* When changing the bin handles when `yshows` is `density` the
  histogram is re-scaled every time the bin handle changes.

* The histogram display supports
  [layers](learn_<R>R</R><Tcl>Tcl</Tcl>_layer.html).
