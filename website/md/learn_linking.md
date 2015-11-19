<script>
window.onload = function() {
    document.getElementById("learn_linking").className += " selected";
    setLearnUrl("linking");
}
</script>


# Introduction

In `loon` terms: linking synchronizes certain `n` dimensional plot
states between linked displays. The user can choose

* which displays should be linked
* which `n` dimensional states should be synchronized
* the mapping of the elements in the `n` dimensional states from one
display to another display.

We allow only one-to-one synchronization. For example, selecting a
point and have it's `k` nearest neighbors automatically selected in
the same display is not possible. Also, it is not possible with the
standard linking mechanism to have one selected point in display A
result in selecting multiple points in display B. For cases where the
standard model is not sufficient custom linking rules can be
implemented with state bindings.

# Loon's Standard Linking Model

The standard linking model is as follows: 

Data displays, i.e. plots, are linked if their `linkingGroup` state
contains the same string.

* Note, `none` is a keyword; if `linkingGroup` equals `none` then the
  plot is not linked.

For example, for

<R>
~~~
p1 <- l_plot(iris[,1:2], linkingGroup='iris')
p2 <- l_plot(iris[,2:3], linkingGroup='iris')
p3 <- l_plot(iris[,3:4], linkingGroup='none')
h <- l_hist(iris[,1], linkingGroup='iris')
~~~
</R>

<Tcl>
~~~
source iris.tcl
set p1 [plot -x $SepalWidth -y $PetalWidth -linkingGroup iris]
set p2 [plot -x $PetalLength -y $PetalWidth -linkingGroup iris]
set p3 [plot -x $SepalLength -y $PetalLength -linkingGroup none]
set h [histogram -x $SepalLength -linkingGroup iris]
~~~
</Tcl>
	
the plots `p1`, `p2`, and `h` are linked to each other and `p3` is not
linked to any other display.


The states of a plot that are linked can be queried with

<R>

~~~
l_getLinkedStates(p1)
~~~

</R>

<Tcl>

~~~
$p1 getLinkedStates
~~~

</Tcl>

Usually the linked states are `color`, `selected`, `active`, and
`size`, if they exist for the particular plot. Histograms, for
example, have no `size` state. Note that between displays only states
with the same state name can be linked. To set the (n-dimensional!)
linked states use

<R>

~~~
l_setLinkedStates(p1, c('color', 'active'))
~~~

</R>

<Tcl>

~~~
$p1 setLinkedStates {color active}
~~~

</Tcl>



By default, the `i`'th element of an `n` dimensional state gets
synchronized with the `i`'th element of the same state in another
displays, if they two displays are linked and if the `i` is not out of
range for one display. Hence, for the above example `n=150`, the
number of data points, and selecting the `i`'th point in `p1` will set
the `selected` state for the `i`'th point in `p2` and `h` to `TRUE`,
for `i=1,...,150`. If a further display gets created with
`linkingGroup` `iris` but a different value for `n`, say `n=4`, then
only the first for elements of that plot get synchronized between that
display and `p1`, `p2`, and `h`. For example, for

<R>
~~~
p4 <- l_plot(1:4,1:4, linkingGroup='iris')
~~~
</R>

<Tcl>
~~~
set p4 [plot -x {1 2 3 4} -y {1 2 3 4} -linkingGroup iris]
~~~
</Tcl>

any change in `p4` for any of the linked states only the first 4
elements in `p1`, `p2`, and `h` will ever be changed. Or vice versa,
only the states of the first for elements in `p1`, `p2`, and `h` will
ever have an effect on `p4`.

It is possible to change the mapping of which elements should be
synchronized between displays. The `n` dimensional `linkingKey` state
controls which elements get synchronized between the linked
displays. That is for two linked displays A and B

* element `i` for the linked states in display A is kept in sync with
  element `j` for the linked states in display B only if the `i`'th
  element in the `linkingKey` state of display A contains exactly the
  same string as the element `j` in the `linkingKey` state of display
  B

* and, the `linkingKey` state for a display must contain unique elements

By default, the `linkingKey` contains an `n` dimensional vector with
`0,1,...,n-1`.




## Switching linkingGroup and linkingKey

Both, the `linkingGroup` state and the `linkingKey` state can be
changed at run time. Changing the `p2` in the above example to have
`linkingGroup` contain the `iris2` string, you can run

<R>
~~~
l_configure(p2, linkingGroup='iris2')
~~~
</R>

<Tcl>
~~~
$p2 configure -linkingGroup iris2
~~~
</Tcl>

now only `p1` and `h` are linked.

If a plot's, say display A, `linkingGroup` is changed to a linking
group that already has linked members, say display B, C, and D, then
one must specify whether the initial synchronization of the linked
states should be a `push`, i.e. A > B, C, D, or a `pull`, i.e. A < B,
C, D. This is done with the `sync` argument:

<R>
~~~
l_configure(p3, linkingGroup='iris2', sync='pull')
~~~
</R>

<Tcl>
~~~
$p2 configure -linkingGroup iris2 -sync pull
~~~
</Tcl>

The `sync` argument must also be used if the `linkingKey` state gets
changed of a plot that has linked displays. For example, to link the
points in `p4` with the last 4 points in the iris data use

<R>
~~~
l_configure(p4, linkingKey=c(146,147,148,149), sync='push')
~~~
</R>

<Tcl>
~~~
$p4 configure -linkingKey {146 147 148 149} -sync push
~~~
</Tcl>


Note that using the default `linkingKey` results in the fastest
linking.

# Custom Linking with State Bindings

If more flexible linking rules are needed, one can implement state
bindings.

For example, for the following displays `pa` and `pb`



<Tcl>

~~~
set pa [plot -x {1 2 3 4 5 6 7} -y {1 2 3 4 5 6 7} -title A]
set pb [plot -x {1 2 3 4 5} -y {1 2 3 4 5} -title B]
~~~

</Tcl>


<R>

~~~
pa <- l_plot(x=1:7, y=1:7, title="A")
pb <- l_plot(x=1:5, y=1:5, title="B")
~~~

</R>

Assume the linking:

![many to one is combined by a logical OR](images/linking_custom1.png "custom linking")


<Tcl>

~~~
proc any {booleans} {
	return [expr [join $booleans ||]]
}


proc pa2pb {pa pb} {
	set sa [$pa cget -selected]
	set sb [$pb cget -selected]
	$pb configure -selected [list\
		[any [lrange $sa 0 2]]\
		[lindex $sa 3]\
		[any [lrange $sa 4 5]]\
		[lindex $sa 6]\
		[lindex $sb 4]]
}

proc pb2pa {pa pb} {
	set sb [$pb cget -selected]
	$pa configure -selected [concat\
		[lrepeat 3 [lindex $sb 0]]\
		[lindex $sb 1]\
		[lrepeat 2 [lindex $sb 2]]\
		[lindex $sb 3]]
}

$pa bind state selected "pa2pb $pa $pb"
$pb bind state selected "pb2pa $pa $pb"
~~~


</Tcl>


<R>

~~~
pa2pb <- function() {
	sa <- pa['selected']
	sb <- pb['selected']
	pb['selected'] <- c(any(sa[1:3]), sa[4], any(sa[5:6]), sa[7], sb[5])
}

pb2pa <- function() {
	sb <- pb['selected']
	pa['selected'] <- c(rep(sb[1],3), sb[2], rep(sb[3],2), sb[4])
}

l_bind_state(pa, 'selected', pa2pb)
l_bind_state(pb, 'selected', pb2pa)
~~~

</R>


This won't end in an endless loop of evaluating state bindings because
after the sequence `pa2pb - pb2pa - pa2pb` or the `pb2pa - pa2pb - 
pb2pa` the system will be in equilibrium with respect to these
functions. Remember that state bindings do not get evaluated if a
`configure` call does not effectively change the state. If, however,
the custom linking does not result in an equilibrium after the first
change, then you may need to add a further variable, say `busy`, to
avoid multiple iterations -- or an infinite loop--. Assume the linking

![directions are indicated by arrows](images/linking_custom2.png "custom linking")


<Tcl>

~~~
set pa2 [plot -x {1 2 3 4 5 6 7} -y {1 2 3 4 5 6 7} -title "A 2"]
set pb2 [plot -x {1 2 3 4 5} -y {1 2 3 4 5} -title "B 2"]

set busy FALSE

proc a2b {pa2 pb2} {
	if {!$::busy} {
		set ::busy TRUE
		$pb2 configure\
			-selected [expr ![lindex [$pa2 cget -selected] 0]]\
			-which_n 0
		set ::busy FALSE
	}
}

proc b2a {pa2 pb2} {
	if {!$::busy} {
		set ::busy TRUE
		$pa2 configure\
			-selected [expr ![lindex [$pb2 cget -selected] 0]]\
			-which_n 0
		set ::busy FALSE
	}
}

$pa2 bind state selected "a2b $pa2 $pb2"
$pb2 bind state selected "b2a $pa2 $pb2"
~~~

</Tcl>


<R>

~~~
pa2 <- l_plot(x=1:7, y=1:7, title="A 2")
pb2 <- l_plot(x=1:5, y=1:5, title="B 2")

busy <- FALSE

a2b <- function() {
	if(!busy) {
		busy <<- TRUE
		sa <- pa2['selected']
		l_configure(pb2, selected=!sa[1], which_n=1)
		busy <<- FALSE
	}
}

b2a <- function() {
	if(!busy) {
		busy <<- TRUE
		sb <- pb2['selected']
		l_configure(pa2, selected=!sb[1], which_n=1)
		busy <<- FALSE
	}
}

l_bind_state(pa2, 'selected', a2b)
l_bind_state(pb2, 'selected', b2a)
~~~

</R>

* Note that the points that are not specified in `which_n` will not be
changed in the `configure` call.

Without the variable `busy` this would end in an infinite loop one the
`selected` state gets changed of either display.
