<script type="text/javascript">
document.getElementById("commands").className += " selected";
</script>

This page in an effort to make the `loon` syntax concise and in some
cases more consistent. For example singular and plural nouns are
sometimes used. All code is given in Tcl. The first order subcommands
for the scatterplot are

~~~
$p configure
$p cget
$p aspect
$p bind
$p systembind
$p destroy
$p glyph
$p layer
$p move
$p scaleto
$p getLinkedStates
$p setLinkedStates
$p info
$p isWidget
~~~


# Layer

Working with plot layers.

~~~
add
        group
	    polygon
	    rectangle
	    oval
	    text
	    image
	    line
	    points


ids
list

getParent
getSiblings
getAncestors
getChildren 
getDescendants
getIndex
getType
getLabel
isVisible
layerVisibility
groupVisibility
hide
show
relabel
bbox
with/use/target
move
delete
expunge
promote
demote
raise
lower
isGroup
~~~

* is `list` good? Because list is a data structure.

* **delete** all subcommands where layer is a verb.

* **use** `layer ids` instead of `layer list`

* **remove** select argument in `info`, `bind change get` etc.

# Glyphs


~~~
add
	images
	text
	serialaxes
	pointrange	
delete 
list

use/with/target

getLabel 
getType
relabel

~~~

* `add images` plural or singular? Glyphs must be always specified for
  each of the $n$ points

# Navigators

## Context


# Bindings

~~~
bind change

bind canvas
bind item

bind layer
bind glyph
bind navigator

$p navigator use navigator0 bind context

navigator use navigator0 bind change
~~~


## Change Binding



## Collection Bindings

### Layer Bindings

### Navigator

#### Context


# Info

~~~
$p info state
$p info debug
~~~


# Aspect

~~~
$p aspect
$p aspect 1
~~~

