<script>
window.onload = function() {
    document.getElementById("learn_states").className += " selected";
    setLearnUrl("states");
}
</script>


# Introduction

All of `loon`'s displays have plot states. Plot states specify what is
displayed, how it is displayed and if and how the plot is linked with
other `loon` plots. `loon`'s plot states are derived from `Tk`'s
configurable options.  A large part of `loon`'s framework revolves
around modifying states, tracking state changes and the
synchronization of plot states between plots.

For example, the plot states of the scatterplot display include `x`,
`y`, `color`, `size`, `selected`, `xlabel`, `ylabel`, `zoomX`,
`zoomY`, `panX`, `panY`, `showScales` and `showGuides`. The
scatterplot display has more than 30 states.


To get a complete list of the plot states for a particular `loon`
widget use the <R>`l_info_states` function</R> <Tcl>`info states`
widget subcommand</Tcl>

<Tcl>

~~~
set p [plot -x {1 2 3} -y {3 2 1}]

$p info states
$p info states x
$p info states {x y xTemp}
~~~

</Tcl>

<R>

~~~
p <- l_plot(iris[,1:2])

istates <- l_info_states(p)
names(istates)

l_info_states(p, 'x')
# or
istates$x

l_info_states(p, c('x', 'y', 'xTemp'))
~~~

</R>

When possible then the data structure for each state is either a
scalar or a flat <R>vector</R><Tcl>list</Tcl>. One exception is the
`data` state which contains a <R>`data.frame`</R><Tcl>`dict`</Tcl>.

# Query and Modify

To query a state, say `showScales`, of the plot `p` use <R> either the
accessor method `[` or the `l_cget` function</R>

~~~	
p['showScales']
l_cget(p, 'showScales')
~~~
</R>

<Tcl>

~~~
$p cget -showScales
~~~

</Tcl>

<R>
To modify a single state, say `showLabels`, use one of

~~~
p['showLabels'] <- FALSE
l_configure(p, showLabels=FALSE)
~~~

to modify multiple states, say `showLabels` and `showScales`,  use

~~~
l_configure(p, showScales=FALSE, showLabels=FALSE)
~~~
</R>

<Tcl>

To modify a single or multiple states use one the `configure` subcommand

~~~
$p configure -showScales TRUE -showLabels TRUE
~~~

</Tcl>


<R>

Note that you should never use the `tkconfigure` function (defined in
the `tcltk` package) instead of the `l_configure` function! One reason
is that the `l_configure` function is customized for `loon` plots and
takes care of some `R` to `Tcl` data structure conversions that are
otherwise not supported (e.g. `data.frame` and nested lists).

</R>



# State Dimension

The dimension of a state is either explicit, i.e. a number, or
abstract, i.e. a letter.

  * Abstract dimensions take on a value at plot creation time.

  * The value of abstract dimensions can be changed when changing the
  dominant states for a dimension together. The dominant states for
  the displays are: `x` for the histogram, `x` and `y` for the
  scatterplot, `data` for the serialaxes display, and for the graph
  display `nodes` is dominant for `n` dimensional states and `from`
  and `to` are dominant for the `p` dimensional states.

<R>
	      p <- l_plot(iris, color=iris$Species)
		  p['n']
		  data(olive)
		  with(olive, l_configure(p, x=oleic, y=stearic, color=Area))
		  l_scaleto_world(p)
		  p['n']
</R>
<Tcl>
	      set p [plot -x {1 2 3} -y {1 2 3}]
		  $p cget -n
		  $p configure -x {1 2} -y {1 2}
		  $p cget -n
</Tcl>

  * Changing an abstract dimension will assign default values to all
    non-dominant states that have that dimension.
  
  * When assigning a single value to a state that has an abstract
    dimension then that value gets repeated accordingly. For example,
    for the `n` dimensional state `selected`
	  
<R>
			p <- l_plot(iris)
			p['selected'] <- TRUE
</R>
<Tcl>
			set p [plot -x {1 2 3} -y {1 2 3}]
			$p configure -selected TRUE
</Tcl>
	will repeat `TRUE` <R>`n=150`</R><Tcl>`n=3`</Tcl> times.
	
  * It is possible to modify a subset of a state with abstract
	dimension using the corresponding `which` argument

<R>
			p <- l_plot(iris)
			l_configure(p, size=15, which_n=as.character(iris$Species) == "setosa")
</R>

<Tcl>
			set p [plot -x {1 2 3} -y {1 2 3}]
			$p configure -size TRUE -which_n {TRUE FALSE TRUE}
</Tcl>

	where `which_*` accepts logical and index sub-setting. <Tcl>In
	`Tcl`, indexing starts at 0</Tcl><R>In `R` indexing starts
	at 1.</R> `which_*` also accepts a state name that is of type
	Boolean and has the same abstract dimension.  <R> l_configure(p,
	active=FALSE, which_n='selected') </R> <Tcl> $p configure -active
	FALSE -which_n selected </Tcl>

	Note that `which_n` is equivalent to `which`.

	* Note that when switching between `R` and `Tcl`: in `R` vector
      indexing starts at 1 and in `Tcl` at 0. The `which...` arguments
      do take this into consideration.
