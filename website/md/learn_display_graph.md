<script>
window.onload = function() {
    document.getElementById("learn_display_graph").className += " selected";
    setLearnUrl("display_graph");
}
</script>

---
title: graph display - loon
---

![](images/display_graph.png "loon graph display")

<R>
~~~
nodes <- LETTERS[1:5]
G <- completegraph(nodes)
LG <- linegraph(G)

g <- l_graph(LG)

l_navigator_add(g)
~~~
</R>

<Tcl>
~~~
set nodes [list A B C D E]
set G [completegraph $nodes]
set LG [linegraph {*}$G]

set g [graph -nodes [lindex $LG 0] -from [lindex $LG 1]\
  -to [lindex $LG 2] -isDirected [lindex $LG 3]]

$g navigator add 
~~~
</Tcl>


# Graph

* A (mathematical) graph in `loon` is defined by a list of node names,
  the from-to list of node names that define the edges and a Boolean
  value whether the graph is directed or not. This translates into the
  states `nodes`, `from`, `to` and `isDirected`.

* The graph layout is defined with the `x` and `y` states.

* Get the state names with

<R>
		states <- l_info_states(g)
		names(states)
</R>

<Tcl>
	    set states [$g info states]
		dict keys $states
</Tcl>

* Query a state, say `background`, as follows

<R>
	    g['background']
</R>

<Tcl>
	    $g cget -background
</Tcl>

* Change a state, say again `background`, `foreground`, and
  `colorEdge`, as follows

<R>
	    g['background'] <- 'gray20'
		g['foreground'] <- 'gray90'
		g['colorEdge'] <- 'red'
		
	alternatively, and more efficient if you modify more than one
    state, use
	
		l_configure(g, background='gray20', foreground='gray90', colorEdge='red')
</R>

<Tcl>
	    $g configure -background gray20 -foreground gray90 -colorEdge red
</Tcl>

* When creating a graph display you may specify any state at plot
  creation

<R>
	    nodes <- letters[1:3]
		G <- completegraph(nodes)
		LG <- linegraph(G)
		g1 <- l_graph(LG, background='gray20', foreground='gray90', colorEdge='red')
</R>

<Tcl>
	    set nodes [list a b c]
		set G [completegraph $nodes]
		set LG [linegraph {*}$G]
		set g1 [graph -nodes [lindex $LG 0] -from [lindex $LG 1]\
			-to [lindex $LG 2] -isDirected [lindex $LG 3]] -colorEdge red
</Tcl>


* details on a state, say `background`, is easily had with

<R>
	    states <- l_info_states(g)
		states$background

	and a particular field
	
		states$background$description
		
</R>

<Tcl>
	    set states [$g info states]
		dict get $states background

	and a particular field

	    dict get $states background description
</Tcl>


# Good To Know

* The graph and scatterplot display are closely related and share most
of the scatterplot states.
* The graph has `n` dimensional states that are associated to nodes
and `p` dimensional states that are associated with edges. To query
which states are `p` dimensional use

<R>
	    states <- l_info_states(g)
		names(Filter(function(x){x$dimension=='p'}, states))
</R>

<Tcl>
	    set states [$g info states]
		set pstates [dict filter $states script {k v} {
			set dim [dict get $v dimension]
			expr {$dim eq "p"}
		}]
		dict keys $pstates	
</Tcl>

* To change the layout of the graph use a graph layout algorithm and
  set the `x` and `y` states of the `loon` graph accordingly. `loon`
  supports straight lines for edges only.

# Navigation Graphs

To turn a graph into a navigation graph you need to add one or more
navigators. Navigators have their own set of states such as `from`,
`to` and `proportion`. You can create state bindings for the navigator
that call a function when a navigator changes its position on the
graph. States and state bindings for navigators provide the facility
to implement any graph semantic. However, certain graph semantics
(e.g. the default semantic with 2d projection along a geodesic path
between spaces) involve lots of logic and control over plots and,
hence, it makes sense to en encapsulate them. We do this by providing
*contexts*. A context is added to a navigator and will do a specific
task if the navigator's position on the graph changes.

## Navigators

We use the example at the beginning of this section:

<R>
~~~
nodes <- LETTERS[1:5]
G <- completegraph(nodes)
LG <- linegraph(G)

g <- l_graph(LG)
~~~
</R>

<Tcl>
~~~
set nodes [list A B C D E]
set G [completegraph $nodes]
set LG [linegraph {*}$G]

set g [graph -nodes [lindex $LG 0] -from [lindex $LG 1]\
  -to [lindex $LG 2] -isDirected [lindex $LG 3]]
~~~
</Tcl>

The following code ads a navigator to the graph `g`


<R>

~~~
nav <- l_navigator_add(g, color='orange')
~~~

</R>


<Tcl>

~~~
set nav [$g navigator add -color orange]
~~~

</Tcl>



![](images/display_graph_navigator1.png "loon graph display")



The navigator with the id  stored in the `nav` has its own states that
can be listed as follows

<R>

~~~
nstates <- l_info_states(nav)
names(nstates)
~~~

</R>


<Tcl>

~~~
set nstates [$g navigator use $nav info states]
dict keys $nstates
~~~

</Tcl>

The position of the navigator on the graph is defined by the states
`from`, `to` and `proportion`. The states `from` and `to` hold
<R>vectors</R><Tcl>lists</Tcl> of node names of the graph. The
`proportion` state is a number between and including `0` and `1` and
defines how far the navigator is between the last element of `from`
and the first element of `to`. The `to` state can also be <R>an empty
string `''`</R><Tcl>an empty list</Tcl> if there is no further node to
go to. Hence, the concatenation of `from` and `to` define a path on
the graph.

## Interaction with the Navigators

The position of the navigator on the graph can be controlled
programatically as follows:


<R>

~~~
l_configure(nav, from=c('A:B','B:C','C:D','A:D'), to=c('D:E','B:E'),
	proportion=0.2)
~~~

</R>


<Tcl>

~~~
$g navigator use $nav configure\
    -from {A:B B:C C:D A:D} -to {D:E B:E}\
    -proportion 0.2
~~~

</Tcl>


![](images/display_graph_navigator2.png "navigator path")


The elements related to the navigator you see in the plot above are

* **Navigator:** wich is `proprtion` between the last node in `from`
  and the first node in `to`. If `to` is empty then the navigator sits
  on the last node of `from`.
* **From Path:** highlighted with a bold line with the same color as
  the navigator shows the path stored in `from` and the `proportion`
  that has been traversed on the current edge.
* **To Path:** highlighted with a thinner line as the from path and
with the same color as the navigator.
* **Path End:** which looks like the navigator (but smaller) and is
located on the last node in `to`.

The graph display supports direct interaction with the navigator and
navigator path using the mouse and keyboard. To move the navigator
with the mouse you must first click on it to select it which will set
the `activeNavigator` graph state to the navigator id and causes the
navigator to be highlighted with the navigator outline in the
selection color.

![](images/display_graph_navigator_select.png "navigator selected")

In this state the following interactions are possible

* Drag the navigator along the path.
* When the navigator is at the path end (i.e. `to=''`) then you can
extend the path by dragging the navigator towards a new connected
node. Note that
	 * the gray circle is the decision boundary to select an adjoining edge.
	 * all the adjoining nodes are highlighted
 ![](images/display_graph_navigator_addtopath.png "add to path")
* Use the scroll wheel to move the navigator along the path. Pay
  attention to the `scrollProportionIncrement` state of the navigator.
* Move the navigator using animation by control double click on a node
  on the path.
* Extend the path by pressing the shift key while selecting adjoining
  nodes to the path end.
* Delete the path and move the navigator to a new node by clicking on
  a node.

Note that the highlighting of the adjacent nodes of a navigator and
the edge selection circle are mouse interaction states and have no
equivalent display states. That is, they are all transient and are
undone as soon as the Shift key and/or mouse button press gets
released.

The animation of the navigator can also be done programatically with
any of the following commands

<R>

~~~
l_navigator_walk_forward(nav)
l_navigator_walk_backward(nav)
l_navigator_walk_forward(nav, 'C:D')
l_navigator_walk_backward(nav, 'B:C')
l_navigator_walk_path(nav, path=c('D:E','B:E','B:D','A:D'))
~~~
</R>


<Tcl>

~~~
$g navigator use $nav walk forward
$g navigator use $nav walk backward
$g navigator use $nav walk forward C:D
$g navigator use $nav walk backward B:C
$g navigator use $nav walk path {D:E B:E B:D A:D}
~~~

</Tcl>

* Note the two navigator states `animationPause` and
`animationProportionIncrement` to control the animation speed.
* You can stop the animation when clicking somewhere, or by scrolling.


## Navigator State Bindings

Navigators support <a
href="learn_<R>R</R><Tcl>Tcl</Tcl>_bind.html#state-bindings">state
bindings.</a> You can use state bindings to implement your custom
navigation graph semantic.

We use the following graph and navigator for our example:

<R>
~~~
nodes <- LETTERS[1:5]
G <- completegraph(nodes)
LG <- linegraph(G)

g <- l_graph(LG)

nav  <- l_navigator_add(g)
~~~
</R>

<Tcl>
~~~
set nodes [list A B C D E]
set G [completegraph $nodes]
set LG [linegraph {*}$G]

set g [graph -nodes [lindex $LG 0] -from [lindex $LG 1]\
  -to [lindex $LG 2] -isDirected [lindex $LG 3]]

set nav [$g navigator add]
~~~
</Tcl>


To add a state binding use

<R>

~~~
my_semantic <- function(widget, navigator) {
	navi <- navigator
	class(navi) <- c("loon", "l_navigator")
	attr(navi, "widget") <- widget
	cat(paste0('do stuff: ', widget, ', ', navigator, ': ',
		tail(navi['from'],n=1),'-',
		round(navi['proportion'],2),'-',
		navi['to'][1],'\n'))
}
		
l_bind_state(nav, event=c('from', 'to', 'proportion'),
	function(W,nav) my_semantic(W,nav))
~~~

</R>

<Tcl>

~~~
proc my_semantic {widget navigator} {
	set p [$widget navigator use $navigator cget -proportion]
	set nf [lindex [$widget navigator use $navigator cget -from] end]
	set nt [lindex [$widget navigator use $navigator cget -to] 0]
	puts [format "do stuff: %s, %s, %s-%.2f-%s" $widget $navigator $nf $p $nt]
}

$g navigator use $nav bind state add {from to proportion}\
	{my_semantic %W %nav}
~~~

</Tcl>


### Substitution

The current substitutions for navigator state bindings are

<R>

argument name| substituted value 
:----:|:----
`W`| widget path name
`nav` | navigator id
`e` | events (states changed)
`b` | binding id


Remember that these substitutions get passed to the `R` function as a
`Tcl` object, hence you need to convert them to the desired type
before using them in your code (e.g. with `l_toR` or `as.numeric`).

</R>


<Tcl>

argument name| substituted value 
:----:|:----
`%W`| widget path name
`%nav` | navigator id
`%e` | events (states changed)
`%b` | binding id
`%O`| canvas path, useful for debugging

</Tcl>



## Contexts


Contexts implement standard graph semantics. Common to all contexts is
that they sign up to the navigators state changes and will evaluate
its (i.e. the context's) `command` state. The contexts add
substitution in the command evaluation that are meaningful for the
particular context. Currently the following contexts are implemented:

* **Context2d** maps every location on a 2d space graph to a list of
`xvars` and a list of `yvars` such that, while moving the navigator
along the graph, as few changes as possible take place in `xvars` and
`yvars`, see the image:

	![](images/context2d.png)

* **Geodesic2d** maps every location on the graph as an orthogonal
  projection of the data onto a two-dimensional subspace. The nodes
  then represent the sub-space spanned by a pair of variates and the
  edges either a 3d- or 4d-transition of one scatterplot into another,
  depending on how many variates the two nodes connected by the edge
  share. The `geodesic2d` context inherits from the `context2d`
  context.


### Geodesic 2d

The following code adds a geodesic2d context to a navigator:

<R>

~~~
G <- completegraph(names(iris[,-5]))
LG <- linegraph(G)

g <- l_graph(LG)
nav <- l_navigator_add(g)

con <- l_context_add_geodesic2d(navigator=nav, data=iris[,-5])
~~~

* Note that the navigator can be either a vector of widget path name
  and navigator id, or the navigator object.
  

</R>


<Tcl>

~~~
source iris.tcl
set G [completegraph {SepalLength SepalWidth PetalLength PetalWidth}]
set LG [linegraph {*}$G]

set g [graph {*}[interleave {-nodes -from -to -isDirected} $LG]]
set nav [$g navigator add]

set con [$g navigator use $nav context add geodesic2d\
	-data [dict filter $iris key Se* Pe*]]
~~~

</Tcl>

This will open a new scatterplot showing the projection defined by the
navigator location. Opening a new scatterplot is the default
behaviour. Every navigator position change will evaluate the command
in the `command` state of the context. The default command state is

<R>

~~~
con['command']

#> [1] ".l2.plot configure -x %x -y %y -xlabel %xlabel -ylabel %ylabel"
~~~

</R>

<Tcl>

~~~
$g navigator use $nav context use $con cget -command

#% .l2.plot configure -x %x -y %y -xlabel %xlabel -ylabel %ylabel
~~~

</Tcl>

where `.l2.plot` is the widget path name of the newly created
scatterplot. If the `command` state is specified at context creation
time, no scatterplot will be created. The `command` state supports
substitutions similar to bindings. The substitution table is

<R>

argument name| substituted value 
:----:|:----
`W`| widget path name (i.e. the graph)
`nav` | navigator id
`con` | context id
`x` | x coordinates of projection 
`y` | y coordinates of projection
`xlabel` | suitable x label for projection
`ylabel` | suitable y label for projection
`from` | from state of navigator
`to`| to state of navigator
`p` | proportion state of navigator

</R>


<Tcl>

string| substituted value 
:----:|:----
`%W`| widget path name (i.e. the graph)
`%nav` | navigator id
`%con` | context id
`%x` | x coordinates of projection 
`%y` | y coordinates of projection
`%xlabel` | suitable x label for projection
`%ylabel` | suitable y label for projection
`%from` | from state of navigator
`%to`| to state of navigator
`%p` | proportion state of navigator

</Tcl>


<R>

Hence, it is easy to use a different scatterplot device, say the
basic `R` plots as follows:

~~~
plot(iris[,1:2], col=iris$Species, pch=16)


con['command'] <- function(x,y,xlabel,ylabel) {
	plot(l_toR(x, as.numeric), l_toR(y, as.numeric), xlab=xlabel, ylab=ylabel,
		col=iris$Species, pch=16, xlim=c(-5,5), ylim=c(-5,5))
}
~~~

Or you can add contour lines of the density estimates

~~~
require(MASS)

con['command'] <- function(x,y,xlabel,ylabel) {
	x <- l_toR(x, as.numeric)
	y <- l_toR(y, as.numeric)

	fit <- kde2d(x,y)

	plot(x, y, xlab=xlabel, ylab=ylabel,
		col=iris$Species, pch=16, xlim=c(-5,5), ylim=c(-5,5))

	contour(fit, add=TRUE)
}
~~~

</R>

The context2d has a couple of noteworthy states, use the `info states`
approach to learn more about them:

<R>

~~~
names(l_info_states(con))

l_info_states(con)$scaling
~~~

</R>


<Tcl>

~~~
set statesInfo [$g use navigator $nav context use $con info states]

dict keys $statesInfo

dict get $statesInfo scaling
~~~

</Tcl>


### Context 2d

The context2d substitutions are

<R>

argument name| substituted value 
:----:|:----
`W`| widget path name (i.e. the graph)
`nav` | navigator id
`con` | context id
`xvars` | x variables
`yvars` | y variables
`from` | from state of navigator
`to`| to state of navigator
`p` | proportion state of navigator

</R>


<Tcl>

string| substituted value 
:----:|:----
`%W`| widget path name (i.e. the graph)
`%nav` | navigator id
`%con` | context id
`%xvars` | x variables
`%yvars` | y variables
`%from` | from state of navigator
`%to`| to state of navigator
`%p` | proportion state of navigator

</Tcl>



If the context2d description <a
href="learn_<R>R</R><Tcl>Tcl</Tcl>_display_graph.html#contexts">above</a>
wasn't clear enough use the following code to get a sense of how
`xvars` and `yvars` change.



<R>

~~~
G <- completegraph(c('A','B','C','D','E','F','G'))
LG <- linegraph(G, separator='-')

g <- l_graph(LG)
nav <- l_navigator_add(g)

foo <- function(xvars, yvars, p) {
	cat(paste0(paste(xvars, collapse=' '), ' to ',
		paste(yvars, collapse=' '), ': ',
		round(as.numeric(p), 3), '\n'))
}

con <- l_context_add_context2d(nav, separator='-',
	command=function(xvars,yvars,p)foo(xvars,yvars,p))
~~~

</R>


<Tcl>

~~~
set G [completegraph {A B C D E F G}]
set LG [linegraph {*}$G "-"]

set g [graph {*}[interleave {-nodes -from -to -isDirected} $LG]]

set nav [$g navigator add]

set con [$g navigator use $nav context add context2d\
	-command {puts "%xvars to %yvars: [format %.3f %p]"} -separator "-"]
~~~

</Tcl>

* Note that the default separator is `:` but it can be changed to any
string in the `linegraph` function and as a context2d state.


The graph can be switched as follows


<R>

~~~
LGnot <- complement(LG)

l_configure(g, nodes=LGnot$nodes, from=LGnot$from,
	to=LGnot$to, isDirected=LGnot$isDirected)
~~~

</R>

<Tcl>

~~~
set LGnot [complement {*}$LG]

$g configure {*}[interleave {-nodes -from -to -isDirected} $LGnot]
~~~

</Tcl>


* Note that the navigator jumps to the first node in the graph if the
  graph changes.


## Graph Switch Widget

Sometimes it is useful to easily switch between different graphs on a
graph display. The graph switch widget maintains a list of graphs and
updates the `activewidget` if a graph in its list gets selected.

For this example we pack a graph switch widget next to a graph
display. More on widget layouts can be read <a
href="learn_<R>R</R><Tcl></Tcl>_layout.html">here</a>.


<R>

~~~
tt <- tktoplevel()
tktitle(tt) <- paste("Loon graph example with a graph switch")

g <- l_graph.default(parent=tt)

gs <- l_graphswitch(activewidget=g, parent=tt)

tkpack(g, side='left', fill='both', expand=TRUE)
tkpack(gs, side='left', fill='y')
~~~

</R>

<Tcl>

~~~
set tt [toplevel .example]
wm title .example "Loon graph example with a graph switch"

set g [graph .example.graph]

set gs [graphswitch .example.graphswitch -activewidget $g]

pack $g -side left -fill both -expand TRUE
pack $gs -side left -fill y
~~~

</Tcl>


![](images/display_graphswitch_empty.png "loon graph and graphswitch")



Graphs are added to the graph switch as follows

<R>

~~~
G1 <- completegraph(LETTERS[1:4])
G2 <- loongraph(nodes=c('a','b','c'), from=c('a','a'),
	to=c('b','c'), isDirected=FALSE)
G3 <- linegraph(G1)
G4 <- complement(G3)

idG1 <- l_graphswitch_add(gs, G1, label='G1')
idG2 <- l_graphswitch_add(gs, G2, label='G2')
idG3 <- l_graphswitch_add(gs, G3, label='G3=linegraph(G1)')
idG4 <- l_graphswitch_add(gs, G4, label='complement(G4)')

l_graphswitch_set(gs, idG3)
~~~

</R>


<Tcl>

~~~
set G1 [completegraph {A B C D}]
set G2 [list {a b c} {a a} {b c} FALSE]
set G3 [linegraph {*}$G1]
set G4 [complement {*}$G2]

set idG1 [$gs add $G1 "G1"] 
set idG2 [$gs add $G2 "G2"]
set idG3 [$gs add $G3 "G3=linegraph(G1)"]
set idG4 [$gs add $G4 "complement(G3)"]

$gs set $idG3
~~~

</Tcl>

![](images/display_graphswitch_selected.png "loon graph and graphswitch")


* To switch a graph select it in the list. Layout, selection, and
  active states are not stored in the graph switch, only the graph
  information.

### Working with the Graph Switch

The API of the graph switch is similar to that of the layers, except
that graphs are arranged in a flat list and layers are arranged in a
tree structure.


<Tcl>

For the graph switch widget the graphs are defined as for the graph
widget with a list of node names and a list with `from` node names and
a list with `to` node names that define the edges, and a logical value
whether the edges are directed or not.

~~~
set gs [graphswitch]

set graphId [$gs add <graph> <label> <index>]
~~~

where `index` is the position in the list. The arguments for `<label>`
and `<index>` are optional.

</Tcl>


<R>

Either graphs of class `loongraph` or class `graph` which is defined
in the `graph` `R` package can be added as follows

~~~
gs <- l_graphswitch()

graphId <- l_graphswitch_add(gs, graph=loongraph(nodes=c('A','B','C'),
	from=c('A','B'), to=c('C','C'), isDirected=FALSE), label='loongraph')

## or

library(graph)
graphId2 <- l_graphswitch_add(gs, graph=randomEGraph(LETTERS[1:15], edges=50),
	label='graph R package graph')
~~~

</R>

The `add` method returns an id for the added graph.

Currently the `activewidget` state of `gs` is not set to any graph
widget. Selecting a graph will throw an error saying that the
graphswitch has no activewidget set. To set an activewidget (i.e. a
graph widget) use

<R>

~~~
g <- l_graph()

gs['activewidget'] <- g
~~~

</R>


<Tcl>

~~~
set g [graph]

$gs configure -activewidget $g
~~~

</Tcl>


Now, to push a graph in `gs` to the graph widget `g` you can either
mouse select a graph on the graphswitch widget or set it
programmatically as follows


<R>

~~~
l_graphswitch_set(gs, id=graphId2)
~~~

</R>

<Tcl>

~~~
$gs set $graphId
~~~

</Tcl>

We continue by adding a few more graphs in order to introduce the
other graphswitch related functions.

<R>

~~~
l_graphswitch_add(gs, graph=randomEGraph(LETTERS[1:15], edges=10))
l_graphswitch_add(gs, graph=randomEGraph(LETTERS[1:15], edges=20))
~~~

</R>


<Tcl>

~~~
$gs add {{A B C D} {A A A} {B C D} FALSE}
$gs add {{A B C D E} {A E A} {B C D} TRUE}
$gs add {{A B C} {A A} {B C} TRUE}
~~~

</Tcl>


To list the ids of all graph in the graphswitch use

<R>

~~~
l_graphswitch_ids(gs)
~~~

</R>

<Tcl>

~~~
$gs ids
~~~

</Tcl>


If you have followed this example then the `ids` method should return
a list with the ids `graph0`, `graph1`, `graph2` and `graph3`, where
the order of the ids is how they appear in the graphswitch widget. To
move a graph to a different position in the list do as follows

<R>

~~~
l_graphswitch_move(gs, id='graph0', index=3)
~~~

</R>

<Tcl>

~~~
$gs move graph0 2
~~~

</Tcl>

to move `graph0` to the second last place. To reorder all graphs use


<R>

~~~
l_graphswitch_reorder(gs, ids=c('graph1', 'graph0', 'graph3', 'graph2'))
~~~

</R>

<Tcl>

~~~
$gs reorder {graph1 graph0 graph3 graph2}
~~~

</Tcl>


To get the label of a graph use

<R>

~~~
l_graphswitch_getLabel(gs, id='graph1')
~~~

</R>

<Tcl>

~~~
$gs getLabel graph1
~~~

</Tcl>

To relabel a graph use

<R>

~~~
l_graphswitch_relabel(gs, id='graph1', label="A special graph")
~~~

</R>

<Tcl>

~~~
$gs relabel graph1 "A special graph"
~~~

</Tcl>

To delete a graph use

<R>

~~~
l_graphswitch_delete(gs, id='graph2')
~~~

</R>

<Tcl>

~~~
$gs delete graph2
~~~

</Tcl>



<R>

And to get the graph as a `loongraph` object use


~~~
l_graphswitch_get(gs, id='graph1')
~~~


</R>

<Tcl>

And to get the graph use

~~~
$gs get graph1
~~~

</Tcl>



<Tcl>

# Graph Utilities

</Tcl>


<R>

# Graph Utilities

The `loon` `R` package comes with functions to create graphs. These
are fairly basic and we recommend to use the algorithms and data
structure from <a
href="http://www.bioconductor.org/packages/release/bioc/html/graph.html">`graph`
`R` package</a>. To coerce between `loongraph` and `graph` object use
the `as.loongraph` and `as.graph` functions. Note that the loongraph
does not contain graph attributes such as edge weights etc. The
`plot.loongraph` method will plot the graph if the `graph` package and
`Rgraphviz` package are loaded.

To create a graph of class `loongraph` use the following function

~~~
G <- loongraph(nodes=c('A','B','C'), from=c('A','A','B'), to=c('B','C','C'),
	isDirected=FALSE)
~~~

Or, to create a complete `loongraph` with the nodes `A`, `B`, and `C`
use

~~~
completegraph(nodes=c('A','B','C'))
~~~

To get the linegraph use

~~~
LG <- linegraph(G, sep="-")
~~~

and the complement

~~~
complement(LG)
~~~

And, given some node names of a graph, on can get the undirected `n`-d
transition graph as follows

~~~
ndtransitiongraph(nodes=c('A:B', 'A:F', 'B:C', 'B:F'), n=3, separator=':')
~~~



# Navigation Graph Workflows

In the `loon` `R` package we provide a couple of convenience functions
to create a navigation graph setup with a single function call.


## l_navgraph

The `l_navgraph` function creates a navigation graph, a graphswitch, a
navigator and a `geodesic2d` context added, and a scatterplot. If the
`graph` argument is not used then a 3d and 4d transition graph and a
complete transition graph is added.

~~~
data(olive)
ng <- l_navgraph(olive[,-c(1,2)], sep='-', color=olive$Area)
~~~

the additional arguments `...` in `l_navgraph` will get passed on to a
configure call for the scatterplot.

* Note that the return value `ng` is a named list with all handles.

![](images/display_graph_navgraph.png "l_navgraph")

## l_ng_ranges

The `l_ng_ranges` produces a graph based on variable pair measures. A
min-max slider is provided to filter variable pairs based on their
associated measures.

~~~
library(scagnostics)

oliveAcids <- olive[,-c(1,2)]
scags <- scagnostics(oliveAcids)

nav <- l_ng_ranges(measures=scags, data=oliveAcids, color=olive$Area)
~~~

![](images/l_ng_ranges_scagnostics.png "l_ng_ranges")
![](images/l_ng_ranges_scagnostics_plot.png "l_ng_ranges")


For more information see the examples of `l_ng_ranges`.

## l_ng_plots

Uses same formal arguments as `l_ng_ranges` but creates a scatterplot
matrix of measures to select the variable pairs.


~~~
library(scagnostics)

oliveAcids <- olive[,-c(1,2)]
scags <- scagnostics(oliveAcids)

nav <- l_ng_plots(measures=scags, data=oliveAcids, color=olive$Area)
~~~

![](images/l_ng_plots_pairs.png "l_ng_plots")


## Measures


It is possible to use `l_ng_ranges` and `l_ng_plots` with arbitrary 1d
and 2d measures.

### Arbitrary Measures

The measures can be arbitrary

~~~
n <- 100
dat <- data.frame(
    A = rnorm(n), B = rnorm(n), C = rnorm(n),
    D = rnorm(n), E = rnorm(n)
)

m2d <- data.frame(
    cor = with(dat, c(cor(A,B), cor(A,C), cor(B,D), cor(D,E), cor(A,E))),
    my_measure = c(1, 3, 2, 1, 4),
    row.names = c('A:B', 'A:C', 'B:D', 'D:E', 'A:E')
)

nav <- l_ng_ranges(measures=m2d, data=dat, separator=':')
~~~

It is important that the `separator` string does not appear in the
variable names.

### Closures of Measures

With the `measures1d` and `measures2d` functions it is possible to
encapsulate the calculation of the measures. In turn, this makes it
possible to recalculate the measures based on a subset of data.

~~~
iqr <- function(x) { diff(quantile(x, probs=c(0.75, 0.25))) }
kurtosis <- function(x) { mean((x-mean(x))^4)/mean((x-mean(x))^2)^2 - 3 }
skewness <- function(x) { mean((x-mean(x))^3)/sd(x)^3 }


s_oliveAcids <- scale(oliveAcids)

m1dc <- measures1d(data=s_oliveAcids, separator='+',
                   median = median,
                   irq = iqr,
                   kurtosis = kurtosis,
                   skewness = skewness)

nav <- l_ng_ranges(measures=m1dc, color=Area)
~~~


Or with 2d scagnostics measures

~~~
library(scagnostics)
oliveAcids <- olive[,-c(1,2)]
scags2d <- scagnostics2d(oliveAcids)

nav <- l_ng_plots(measures=scags2d, color=olive$Area)
~~~



</R>
