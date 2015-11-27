<script type="text/javascript">
window.onload = function() {
    document.getElementById("learn_bind").className += " selected";
    setLearnUrl("bind");
}
</script>

---
title: event bindings - loon
---

# Event Bindings


Event bindings provide the functionality of binding code to specific
event types. The bound code is called a *callback*. In `loon`, we
distinguish between four classes of events: state change events, item
events, canvas events and content events. Examples of each type of
event (in the same order as mentioned before) include: a selected
state modification of a plot, moving the mouse cursor over a point
glyph, re-sizing the plot window and adding a layer.

For example, a state binding is triggered on particular state changes


<R>

~~~
p <- l_plot(x=1:3, y=3:1)

l_bind_state(p, event=c('selected', 'active', 'xTemp'),
	callback=function(W, e) {cat(paste(W, 'had events:', e, '\n' ))})
~~~

</R>


<Tcl>

~~~
set p [plot -x {1 2 3} -y {3 2 1}]

$p bind state add {selected active xTemp} {puts "%W had events: %e"}
~~~

</Tcl>

The above code creates a plot with three points and adds a state
binding that evaluates the callback <Tcl>code `puts "%W had event
%e"`</Tcl><R>function</R> if any of the states `selected`, `active` or
`xTemp` get changed of `p`. <Tcl>The `%W` and `%e` get **substituted**
with</Tcl><R>The arguments `W` and `e` contain</R> the widget path
name and a <Tcl>list</Tcl><R>vector</R> of the events that were
responsible for the code to be evaluated, respectively.



There are also a number of other bindings:

* Item bindings evaluate code triggered by a keyboard and mouse
  gesture with a visual item on the display.
* Canvas bindings evaluate code triggered by a keyboard and mouse
  gesture on the plot as a whole.
* Content bindings evaluate callbacks when the collection of layers,
  glyphs or navigators changes.

<Tcl> Besides the `bind` subcommand we also provide the `systembind`
subcommand. `bind` and `systembind` can be used
interchangeably. `systembind` is used for `loon`'s own use and no
error catching is performed when evaluating system binding code.
</Tcl>


<R>

## R Callbacks

The `callback` argument of the bind functions expects an `R` function
which is called by the `Tcl` interpreter if the event of interest
happens. The `Tcl` interpreter can call `R` functions using their
hex-encoded address as follows

~~~
foo <- function(x,y,a,b) {	
	s <<- sum(sapply(c(x,y,a,b), as.numeric)^2)
}

.Tcl.callback(foo)

#> [1] "R_call 0x7fe6965adb78 %x %y %a %b"
~~~

The `R_call` procedure in `Tcl` uses the hex-address to access the `R`
function. The `x`, `y`, `a` and `b` arguments of `foo` get translated
to `%x`, `%y`, `%a` and `%b`. The `%` arguments are meant to be
substituted by `Tcl` before the `R_call` evaluation. Note, that `Tcl`
will always send strings to the arguments of the `R` function and
hence they need to changed to the required type as we did with the
`as.numeric` `R` function. For example

~~~{.notrun}
.Tcl('R_call 0x7fe6965adb78 1 2 3 4')

s
#> 30
~~~

If `Tcl` submits a `Tcl` list as an argument then in the `R` callback
this list appears as a string with the list elements separated by a
space. We provide the `l_toR` function to convert a `R` callback
arguments to vectors of a specific types. For example `l_toR(e,
as.numeric)` converts `e` to an `R` vector of numbers.


`loon` supports a number of substitutions such as `%W`, `%e` and `%b`
for widget path name, events and binding id,
respectively. Substitutions are optional and hence any combination of
the substitution strings and hence argument names of the `R` function
can be used. In other words

~~~
l_bind_state(p, 'all', function() {})
l_bind_state(p, 'all', function(W) {})
l_bind_state(p, 'all', function(e, W) {})
l_bind_state(p, 'all', function(e, W, b) {})
l_bind_state(p, 'all', function(b) {})
~~~

are all valid `R` functions to be evaluated by a state binding. This
is the same mechanism as for the standard `Tk` widget bindings.

Note that the hex-encoded address points to a particular `R` function
and not the function name. Hence if you re-define an `R` function and
assign it to the variable (i.e function name) then the `Tcl`
interpreter will not use that new function. For example

~~~
foo <- function(W) {cat(paste(W, 'had some event\n'))}
p <- l_plot(1:3,1:3)
l_bind_state(p, 'all', foo)

foo <- function(W) {cat(paste('There were some events in widget ', W, '\n'))}

p['selected'] <- TRUE

#> .l0.plot had some event
~~~

The easiest way to deal with this to wrap the `foo` function call into
another anonymous function

~~~
foo <- function(W) {cat(paste(W, 'had some event\n'))}
p <- l_plot(1:3,1:3)
l_bind_state(p, 'all', function(W){foo(W)})

foo <- function(W) {cat(paste('There were some events in widget', W, '\n'))}

p['selected'] <- TRUE

#> There were some events in widget .l0.plot 
~~~

</R>


## State Change Bindings

State bindings get triggered when widget states change. For more
information on widget states see the <a
href="learn_<R>R</R><Tcl>Tcl</Tcl>_states.html">states
documentation</a>.

A `configure` call that changes multiple states will collect which
states have changed and only evaluate the change bindings once. For
example

<R>

~~~
p <- l_plot(x=1:3, y=3:1)

l_bind_state(p, event=c('selected', 'active', 'xTemp'),
	callback=function(W,e) {
		cat(paste(W, 'had events:', e, '\n' ))
		})

l_configure(p, selected=c(TRUE, FALSE, FALSE), size=c(1, 4, 3))

#> .l0.plot had events: selected size

p['xTemp'] <- c(1,1,1)

#> .l0.plot had events: xTemp
~~~

</R>

<Tcl>

~~~
set p [plot -x {1 2 3} -y {3 2 1}]

$p bind state add {selected active xTemp} {puts "%W had events: %e"}

$p configure -selected {T F F} -size {1 4 3}

#% .l0.plot had events: selected size

$p configure -xTemp {1 1 1}

#% .l0.plot had events: xTemp
~~~

</Tcl>

A couple of notes:

- The events <Tcl>`{selected active xTemp}`</Tcl> <R>`c('selected',
'active', 'xTemp')`</R> in the binding creation specify that *any* of
these events trigger the code evaluation.  Or in other words, *at
least one* of the `selected`, `active`, or `xTemp` states must be
changed to trigger the code evaluation.
- The <Tcl>`%e` substitution</Tcl><R>`e` argument</R> is a list with
  every state that got changed in the particular `configure`
  evaluation.
- The keyword `all` for events indicates that every state change
  should trigger a callback evaluation.
- The event `destroy` is thrown when a plot gets closed (destroyed).

A state binding gets thrown only if at least one state is really
changed. Suppose at least one point is not selected, then

<Tcl>

~~~
$p configure -selected 1
#% .l0.plot had events selected
$p configure -selected TRUE
~~~

</Tcl>

<R>

~~~
p['selected'] <- TRUE
#> .l0.plot had events selected
p['selected'] <- TRUE
~~~

</R>

The above code will only throw one event for the first expression and
no event for the second expression, as the `selected` state does not
change <Tcl>(`1` and `TRUE` both represent the same logical
value)</Tcl>.


### Substitutions

The current substitutions for state bindings are

<R>

argument name| substituted value 
:----:|:----
`W`| widget path name
`e` | states that got changed
`b` | binding id

Remember that these substitutions get passed to the `R` function as a
`Tcl` object, hence you need to convert them to the desired type
before using them in your code (e.g. with `l_toR` or `as.numeric`).

</R>


<Tcl>

string| substituted value 
:----:|:----
`%W`| widget path name
`%e` | states that got changed
`%b` | binding id
`%O`| object path, useful for debugging

</Tcl>


### List, Reorder \& Delete Bindings

Assume the following plot and bindings



<R>

~~~
p <- l_plot(1:3, 1:3)
l_bind_state(p, 'all', function(){cat('A\n')})
l_bind_state(p, c('selected', 'active'), function(){cat('B\n')})
l_bind_state(p, c('showAxes', 'selected'), function(){cat('C\n')})
l_bind_state(p, c('zoomX', 'panX', 'selected'), function(){cat('D\n')})
~~~

</R>


<Tcl>

~~~
set p [plot -x {1 2 3} -y {1 2 3}]
$p bind state add all {puts A}
$p bind state add {selected active} {puts B}
$p bind state add {showAxes selected} {puts C}
$p bind state add {zoomX panX selected} {puts D}
~~~

</Tcl>


To list the bindings use

<R>

~~~
l_bind_state_ids(p)

#> [1] "stateBinding0" "stateBinding1" "stateBinding2" "stateBinding3"
~~~

</R>


<Tcl>

~~~
$p bind state ids

#% stateBinding0 stateBinding1 stateBinding2 stateBinding3
~~~

</Tcl>

To get the binding events and callback say for `stateBinding0` use

<R>

~~~
l_bind_state_get(p, 'stateBinding0')

#> [1] "all"              "R_call 0x1e356e8"
~~~

</R>


<Tcl>

~~~
$p bind state get stateBinding0

#% all {puts A}
~~~

</Tcl>

The order of binding evaluation is as returned by <Tcl>bind state
ids</Tcl><R>l_bind_state_ids</R> for those bindings that are triggered
by a particular state change. For the above example all bindings are
triggered by a `selected` state change, hence, changing the selected
state of `p` would print `A`, `B`, `C` and `D`. The evaluation can be
reversed/changed as follows

<R>

~~~
l_bind_state_reorder(p, rev(l_bind_state_ids(p)))
~~~

</R>

<Tcl>

~~~
$p bind state reorder [lreverse [$p bind state ids]]
~~~

</Tcl>

Now a change of the `selected` state of `p` would cause to print the
letters `D`,. `C`, `B`, `A`.


Finally, to delete a binding use

<R>

~~~
l_bind_state_delete(p, 'stateBinding0')
~~~

</R>


<Tcl>

~~~
$p bind state delete stateBinding0
~~~

</Tcl>

### Other State Bindings

Next to a `loon`'s displays the layers, glyphs, navigators and
contexts support state bindings too. We now demonstrate this with a
layer state binding (note that this it will work with all the other
types the same).

<R>

~~~
p <- l_plot(1:3,1:3)
l <- l_layer_rectangle(p, x=c(1,3), y=c(1,3), color='blue')
l_bind_state(l, 'color', function(){cat('layer color has changed\n')})
l['color'] <- 'green'

#> layer color has changed
~~~

</R>


<Tcl>

~~~
set p [plot -x {1 2 3} -y {1 2 3}]
set l [$p layer rectangle -x {1 3} -y {1 3} -color blue]
$p layer use $l bind state add color {puts "layer color has changed"}
$p layer use $l configure -color green

#% layer color has changed
~~~

</Tcl>

## Item Bindings

Item bindings are triggered by a mouse/keyboard gesture over a visual
item in a plot. Visual items include point glyphs, layers, axes and
labels. Every visual item has a set of tags as outlined in the
[Visual Item Tags][] section. In addition, displays and layers have a
`tag` state for user specified tags.

Note that item bindings are `Tk` canvas bindings with one level of
indirection in order to support loon's context specific substitutions
instead of the standard `Tk` substitutions.

Valid event patterns for mouse/keyboard gestures are taken from the
[`Tk` bindings](http://www.tcl.tk/man/tcl8.6/TkCmd/bind.htm#M7). The
tag specification for item bindings allow for logical expressions of
[Visual Item Tags][] using the operators `&&`, `||`, `^`,`!`, and
parenthesized subexpressions (see the
[`Tk` canvas manual](https://www.tcl.tk/man/tcl8.6/TkCmd/canvas.htm#M16)).

To get the tags for the item that lies below the mouse cursor use the
<R>`l_currenttags` function</R><Tcl>`currenttags` subcommand</Tcl>. To
get the index (in relation to the abstract dimension) of the visual
item below the mouse cursor (if there is an index) use the
<R>`l_currentindex` function</R><Tcl>`currentindex` subcommand</Tcl>.

For example, say we wish to print out the point number in a
scatterplot on leaving and entering the point

<R>

~~~
p <- l_plot(iris[,1:2], color=iris$Species)

printEntered <- function(W) {
	cat(paste('Entered point ', l_currentindex(W), '\n'))
}

printLeave <- function(W) {
	cat(paste('Left point ', l_currentindex(W), '\n'))
}

l_bind_item(p, tags='model&&point', event='<Enter>',
	callback=function(W) {printEntered(W)})

l_bind_item(p, tags='model&&point', event='<Leave>',
	callback=function(W) {printLeave(W)})
~~~

</R>


<Tcl>

~~~
set p [plot -x {1 2 3} -y {1 2 3}]

$p bind item add "model&&point" <Enter> {puts "Entered point [%W currentindex]"}
$p bind item add "model&&point" <Leave> {puts "Left point [%W currentindex]"}
~~~

</Tcl>

The item binding API also support
[List, Reorder \& Delete Bindings][], but the item binding order has
currently no effect.


### Substitutions

The current substitutions for item bindings are

<R>

argument name| substituted value 
:----:|:----
`W`| widget path name
`b` | binding id
`x` | x coordinate 
`y` | y coordinate

Remember that these substitutions get passed to the `R` function as a
`Tcl` object, hence you need to convert them to the desired type
before using them in your code (e.g. with `l_toR` or `as.numeric`).

</R>


<Tcl>

string| substituted value 
:----:|:----
`%W`| widget path name
`%b` | binding id
`%O`| canvas path, useful for debugging
`%x` | x coordinate 
`%y` | y coordinate

</Tcl>

### Visual Item Tags


Visual items have tags. It is possible to add user defined tags with
the `tag` state for the relative object (i.e. plot and layer). There
are, however, also tags that we use which are listed in the table
below.

* **Note:** numbers are not valid tags! Tags must start with a
  alphabetic character.

It is possible to query the item tags interactively with a mouse
button press on an item as follows

<R>

~~~
printTags <- function(W) {
	print(l_currenttags(W))
}

p <- l_plot(x=1:3, y=1:3, title='Query Visual Item Tags')

l_bind_item(p, 'all', '<ButtonPress>', function(W)printTags(W))
~~~

</R>


<Tcl>

~~~
set p [plot -x {1 2 3} -y {1 2 3} -xlabel xlab\
	-ylabel ylab -title "Query Visual Item Tags"]

$p bind item add all <Enter> {puts "[%W currenttags]"}
~~~

</Tcl>

* `all`, `current` and `selected` are reserved tags used by `Tk`


The current tagging scheme for the histogram, scatterplot and graph
displays is


![](images/item_tags.png "Item Tags")



## Canvas Bindings


Canvas bindings are in contrast to the item bindings triggered by a
mouse/keyboard gesture over the plot as a whole. Canvas bindings are
for example useful if one wants to capture plot resize events or a
mouse moving events.

As for item bindings the valid event patterns for mouse/keyboard
gestures are taken from the
[`Tk` bindings](http://www.tcl.tk/man/tcl8.6/TkCmd/bind.htm#M7).


For the first example, we print the size of the plot when it gets
resized. 

<R>

~~~
p <- l_plot(iris[,1:2], color=iris$Species)

printSize <- function(p) {
	size <- l_size(p)
	cat(paste('Size of widget ', p, ' is: ',
		size[1], 'x', size[2], ' pixels\n', sep=''))	
}

l_bind_canvas(p, event='<Configure>', function(W) {printSize(W)})
~~~


Note that the size could also be passed as an argument (`w` and `h`)
for the callback function.

</R>

<Tcl>

~~~
set p [plot -x {1 2 3} -y {1 2 3}]

proc printSize {widget width height} {
	puts [format "Size of widget %s is %sx%s pixels" $widget $width $height]
}

$p bind canvas add <Configure> {printSize %W %w %h}
~~~

</Tcl>

Or, say, we want to track the mouse and print out its location in data
coordinates

<R>

~~~
p <- l_plot(iris[,1:2], color=iris$Species)

printLocation <- function(W,x,y) {
	cat(paste('In widget ', W,
		' the location of the mouse cursor is at: ',
		round(l_toR(x, as.numeric),3), ' and ',
		round(l_toR(y, as.numeric),3), '\n', sep=''))	
}

l_bind_canvas(p, event='<Motion>', printLocation)
~~~

</R>

<Tcl>

~~~
set p [plot -x {1 2 3} -y {1 2 3}]

proc printLocation {widget x y} {
	puts [format "In widget %s the location of the mouse cursor is at: %s and %s"\
		$widget $x $y]
}

$p bind canvas add <Motion> {printLocation %W %x %y}
~~~

</Tcl>



### Substitutions

The current substitutions for canvas bindings are

<R>

argument name| substituted value 
:----:|:----
`W`| widget path name
`b` | binding id
`x` | x coordinate 
`y` | y coordinate
`w` | plot width in pixel
`h` | plot height in pixel

Remember that these substitutions get passed to the `R` function as a
`Tcl` object, hence you need to convert them to the desired type
before using them in your code (e.g. with `l_toR` or `as.numeric`).

</R>


<Tcl>

string| substituted value 
:----:|:----
`%W`| widget path name
`%b` | binding id
`%O`| canvas path, useful for debugging
`%x` | x coordinate 
`%y` | y coordinate
`%w` | plot width in pixel
`%h` | plot height in pixel

</Tcl>



## Content Bindings

There are also layer, glyph, navigator and context bindings. These bindings get
evaluated if the collection of one of those changes. For example

<R>

~~~
p <- l_plot(x=1:3, y=1:3)

l_bind_layer(p, c('add', 'delete'), function(W,l,e) {
	cat(paste('Widget', W, 'had event', e, 'for layer:', l, '\n'))
	})

l <- l_layer_texts(p, x=c(2,2), y=c(1.5, 2.5), text=c('A','B'))

#> Widget .l0.plot had event add for layer layer1
~~~

</R>


<Tcl>

~~~
set p [plot -x {1 2 3} -y {1 2 3}]

$p bind layer add {add delete} {puts "Widget %W had event %e for layer: %l"}

$p layer texts -x {2 2} -y {1.5 2.5} -text {A B}

#% Widget .l0.plot had event add for layer layer1
~~~

</Tcl>


Valid events for the different types are

* layer: `all`, `add`, `delete`, `move`, `relabel`, `hide`, `show`
* glyph: `all`, `add`, `delete`, `relabel`
* navigator: `all`, `add`, `delete`, `relabel`
* context: `all`, `add`, `delete`, `relabel`

