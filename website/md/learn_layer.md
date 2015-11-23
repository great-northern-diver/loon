
<script>
window.onload = function() {
    document.getElementById("learn_layer").className += " selected";
    setLearnUrl("layer");
}
</script>

---
title: learn layering information - loon
---



# Introduction

`loon`'s displays that are based on Cartesian coordinates
(i.e. scatterplot, histogram and graph display) allow for layering
visual information including polygons, text and rectangles. Every
layer has a unique id and the layer with the plot model
(i.e. scatterplot points, histogram or graph) is called the model
layer and has the id `model`.

The available layer types are the following

* **group**: a layer that has no visual representation or states but
  can contain other layers
* **polygon**: a single polygon
* **line**: a single line with multiple segments
* **rectangle**: a single rectangle
* **oval**: a single oval
* **text**: a single string

and `n` dimensional state or compound layers

* **points**: `n` circles (with `size` state)
* **texts**: `n` strings
* **polygons**: `n` polygons
* **rectangles**: `n` rectangles
* **lines**: `n` lines


Note that for polygons, rectangles and lines the states `x` and `y`
have a non-flat data structure <R>, i.e. an `R` they use a list of
vectors as follows

~~~
l_layer_polygons(p, x=list(c(1,2,3), c(4,2,1), c(1,2,3)),
	y=list(c(2,5,3), c(2,7,4), c(4,8,1)))
~~~
</R>	
<Tcl>, i.e. a nested list

~~~
$p layer polygons -x {{1 2 3} {4 2 1 3} {1 2 3 4 1}}\
    -y {{2 1 3} {2 1 4 3} {2 1 4 3 2}}\
	-color {red red blue}\
	-active {T F T}\
	-selected {T T T}
~~~
</Tcl>				


Some important implementation details for working with layers are

* Currently only the model layer is interactive in the sense of
  selection and moving elements with mouse gestures.
* Every layer has a unique id that is assigned by loon.
* Layers are arranged in a tree structure where a `group` layer can be
  a parent to children layers (any of the above mentioned layer
  types). The tree root has id `root`.
* The layers are rendered in order of a Depth-first traversal of the
  non-group layers.
* Layers have states that define its appearance.
	* Layer states support state bindings.
* Sometimes (e.g. for texts layers), `n` can not be set to 1. Use the
  singular version instead (e.g. text).

<R>

To get a first impression on the possible operations that can be
performed on layers you may query all commands that are available for
working with layers

~~~
apropos("l_layer_")
~~~

</R>


# Add, Move & Delete Layers

In this section we layer information onto the following scatterplot

<Tcl>

~~~
set p [plot -x {0 1 2 3 4 5 6 7} -y {0 1 2 3 4 5 6 7}\
	-showScales TRUE -showGuides TRUE]
~~~

</Tcl>

<R>

~~~
p <- l_plot(x=0:7, y=0:7, showScales=TRUE, showGuides=TRUE,
	xlabel='', ylabel='')
~~~

</R>

![](images/layer_init.png)

The `layer ids` sub-command  returns the plot's layer ids

<Tcl>

~~~
$p layer ids

#% root model
~~~

</Tcl>


<R>

~~~
l_layer_ids(p)

#> [1] "root"  "model"
~~~

</R>


The `root` and `model` layer exist in all plots. The `root` layer is
the tree root and the `model` layer represents the visual
representation of the data for the specific plot (e.g. histogram,
scatterplot or graph).

The following code layers a polygon

<Tcl>

~~~
set l_p [$p layer polygon -x {0 1 2 3 3 2.5 1.5 0}\
    -y {5 4.5 4.5 5 7 7 5.5 5.2}\
    -color black -linecolor orange\
    -linewidth 5]
~~~

</Tcl>


<R>

~~~
l_p <- l_layer_polygon(p, x=c(0,1,2,3,3,2.5,1.5,0),
	y=c(5,4.5,4.5,5,7,7,5.5,5.2),
	color='black', linecolor='orange',
	linewidth=5)
~~~

</R>

![](images/layer_polygon.png)

The variable `l_p` holds the layer id. <R>In `R` the `l_p` has in
addition a class and widget attribute.</R> You can get the state
descriptions <Tcl>similar </Tcl>as with normal plots

<Tcl>

~~~
$p layer use $l_p info states
~~~

</Tcl>

<R>

~~~
l_info_states(l_p)
~~~

</R>

Other layer types can be layered similarly, e.g.

<Tcl>

~~~
set l_l [$p layer line -x {0 2.5 5 7.5}\
	-y {4.5 0 6 3}\
	-linewidth 4\
	-color red]
~~~

</Tcl>

<R>

~~~
l_l <- l_layer_line(p, x=c(0,2.5,5,7.5), y=c(4.5,0,6,3),
	linewidth=4, color='red')
~~~

</R>


![](images/layer_line.png)

The layers are arranged in a tree structure and the rendering is
according to the Depth-First algorithm of the visual layers in the
tree. For example, we can layer a rectangle over the previous layers:

<Tcl>

~~~
set l_r [$p layer rectangle\
	-x {0.5 6.5} -y {0.5 6.5} -color green -linecolor '']
~~~

</Tcl>


<R>

~~~
l_r <- l_layer_rectangle(p, x=c(0.5,6.5), y=c(0.5,6.5),
	color='green', linecolor='')
~~~

</R>

![](images/layer_rectangle.png)

The rectangle with the layer id saved in the `l_r` variable over-plots
the other layers, i.e. it is rendered last. To get a printout of the
tree structure run

<Tcl>

~~~
$p layer printTree

#% layer2
#% layer1
#% layer0
#% model
~~~

</Tcl>


<R>

~~~
l_layer_printTree(p)

#> layer2
#> layer1
#> layer0
#> model
~~~

</R>

Hence the topmost layer, i.e. `layer2`, is rendered last. Layers can
be moved with the <Tcl>`layer move`</Tcl> <R>`l_layer_move`</R>
function as follows

<Tcl>

~~~
$p layer move $l_r root end
~~~

</Tcl>


<R>

~~~
l_layer_move(p, layer=l_r, parent='root', index='end')
~~~

</R>

![](images/layer_move_1.png)

Note, that the `parent` layer, the `index` specifying the location
among the parents children layers, and the `label` of a layer can also
be specified when adding a layer. However `parent`, `index` and
`label` are not states of the layer, instead they are information for
the layer collection.

The following code creates a group and moves the polygon layer and
line layer into it

<Tcl>

~~~
set l_g [$p layer group -parent root -index end]
$p layer move $l_l $l_g end
$p layer move $l_p $l_g end
$p layer printTree

#% layer3
#% model
#% layer2
#% +layer3
#%   layer1
#%   layer0
~~~

</Tcl>

<R>

~~~
l_g <- l_layer_group(p, parent='root', index='end')
l_layer_move(p, layer=l_l, parent=l_g, index='end')
l_layer_move(p, layer=l_p, parent=l_g, index='end')
l_layer_printTree(p)

#> layer3
#> model
#> layer2
#> +layer3
#>   layer1
#>   layer0
~~~

</R>

![](images/layer_group.png)

To move a layer one position up or down (i.e. change place with a
sibling) one can also use the <R>`l_layer_raise` and `l_layer_lower`
function</R><Tcl>`layer raise` and `layer lower` sub command</Tcl>,
respectively.


The visibility of a layer can be changed with the <Tcl>`hide` and
`show` sub-command</Tcl> <R>`l_layer_hide` and `l_layer_show`
function</R>.

<Tcl>

~~~
$p layer hide $l_g
~~~

</Tcl>


<R>

~~~
l_layer_hide(p, l_g)
~~~

</R>

![](images/layer_hide.png)

and

<Tcl>

~~~
$p layer show $l_g
~~~

</Tcl>


<R>

~~~
l_layer_show(p, l_g)
~~~

</R>

![](images/layer_show.png)

The layer with the rectangle can be deleted as follows

<Tcl>

~~~
$p layer delete $l_r
~~~

</Tcl>


<R>

~~~
l_layer_delete(p, l_r)
~~~

</R>

![](images/layer_delete.png)


If a group layer gets deleted with <R>`l_layer_delete`</R><Tcl>`layer
delete`</Tcl> then all its children layers get moved into their
grandparent group layer. To delete a group layer and all it's children
use the <R>`l_layer_expunge` function</R><Tcl>`layer expunge`
subcommand</Tcl>.


<Tcl>

~~~
$p layer expunge $l_g
~~~

</Tcl>


<R>

~~~
l_layer_expunge(p, l_g)
~~~

</R>

![](images/layer_expunge.png)


It is also possible zoom and pan such that a particular layer fills
the plot region


<R>

~~~
l_o <- l_layer_oval(p, x=c(2.5,5), y=c(2.5,5), color='thistle', index='end')
l_scaleto_layer(p, l_o)
~~~

</R>

<Tcl>

~~~
set l_o [$p layer oval -x {2.5 5} -y {2.5 5} -color thistle -index end]
$p scaleto layer $l_o
~~~

</Tcl>


![](images/layer_scaleto.png)

# Query and Modify Layers

To modify the layer states works as described for plot states <a
href='learn_<R>R</R><Tcl></Tcl>_states.html'>here</a>. We start with
the following histogram with a polygon layer:

<Tcl>

~~~
set h [histogram -x {1 1 2 1 4 3 2 2 1 4 5 4 3 2 4 3}\
	-binwidth 0.85 -showScales TRUE -showLabels FALSE]

set l_p [$h layer polygon -x {2 3 4 4.5 4 3.8 2.2}\
	-y {0.1 0 1 3 2 4 5}\
	-color steelblue -linecolor '']

$h scaleto world
~~~

</Tcl>

<R>

~~~
h <- l_hist(x=c(1,1,2,1,4,3,2,2,1,4,5,4,3,2,4,3), binwidth=0.85,
	showScales=TRUE, showLabels=FALSE)

l_p <- l_layer_polygon(h, x=c(2,3,4,4.5,4,3.8,2.2),
	y=c(0.1,0,1,3,2,4,5), color='steelblue', linecolor='')

l_scaleto_world(h)
~~~

</R>


![](images/layer_histogram.png)


To query the state information use

<R>

~~~
l_info_states(l_p)
~~~

</R>

<Tcl>

~~~
$h layer use $l_p info states
~~~

</Tcl>



A layer state is queried as follows:

<Tcl>

~~~
$h layer use $l_p -color
~~~

</Tcl>

<R>

* If `l_p` is of class `l_layer`

		l_p['color']

	or for multiple state changes

		l_cget(l_p, 'color')

* Or generally where `l_p` and `h` are only expected to be strings
  without a special class

		l_cget(c(h, l_p), 'color')

</R>



A layer state is configured as follows:

<Tcl>

~~~
$h layer use $l_p configure -color red -linecolor black -linewidth 2
~~~

</Tcl>


<R>

* If `l_p` is of class `l_layer`

		l_p['color'] <- 'red'

	or for multiple state changes

		l_configure(l_p, color='red', linecolor='black')

* Or generally where `l_p` is only expected to be a string without a
  special class

		l_configure(c(h, l_p), linewidth=2)

</R>

![](images/layer_configure.png)




<R>

# Maps

The `loon` `R` package also supports layering maps of classes defined
in the `sp` and `maps` `R` packages. For a general overview of map
data in `R` take a look at the <a
href="https://cran.r-project.org/web/views/Spatial.html">CRAN Task
View: Analysis of Spatial Data</a>.

If you use the `asSingleLayer=FALSE` argument `loon` will create
multiple individual polygon and line layers within a group. The
default `asSingleLayer=TRUE` option will return a single polygons or
lines layer. The default behavior is recommended as it keeps the
displays faster.

## maps library

We start with maps in the `maps`. First we create a scatterplot with
points located at the coordinates of Canadian citites

~~~
library(maps)
data(world.cities)

canada.cities <- subset(world.cities,
    grepl("canada", country.etc , ignore.case=TRUE))

p <- with(canada.cities,l_plot(x=long, y=lat, showLabels=FALSE))

g_t <- l_glyph_add_text(p, text=canada.cities$name)
p['glyph'] <- g_t

~~~


![](images/layer_map_cities.png)

The canada regions are then layered as follows:

~~~
canada.map <- map("world",  "Canada", fill=TRUE, plot=FALSE)

id <- l_layer(p, canada.map,
	color = ifelse(grepl("lake", canada.map$names,
	   ignore.case=TRUE), "lightblue", ""),
	asSingleLayer=FALSE)

l_scaleto_layer(p, id)
~~~

![](images/layer_map_canada.png)

* Note that if you would like every polygon to be an individual layer,
  you can use the `asSinglelLayer=FALSE` argument in the `l_layer.map` method.

## sp library

Of the classes currently defined in the `sp` package for geographical
data we currently support to layer of class `Polygon`, `Polygons`,
`SpatialPolygons`, and `SpatialPolygonsDataFrame`. There are a couple
of sources that provide map data for `R` using these classes, see

* <a href="http://www.gadm.org">Global Administrative Areas</a>
* <a href="http://www.naturalearthdata.com">Natural Earth</a>

This example uses data from the <a href="http://www.gadm.org">Global
Administrative Areas</a>. We start by layering an outline of
Switzerland into a scatterplot with 0 points:

~~~
con <- url("http://biogeo.ucdavis.edu/data/gadm2/R/CHE_adm0.RData")
load(con)
close(con)
	
p <- l_plot()
g <- l_layer_group(p, label="Switzerland")
m <- l_layer(p, gadm, label="Switzerland", parent=g,
	         asSingleLayer=FALSE,
             color="", linecolor="black")
l_scaleto_world(p)
~~~

![](images/layer_map_switzerland.png)

We continue by layering the outlines for the Swiss Cantons:

~~~
l_layer_hide(p, g)

g1 <- l_layer_group(p, label="Swiss Cantons")

con <- url("http://biogeo.ucdavis.edu/data/gadm2/R/CHE_adm1.RData")
load(con)
close(con)

m1 <- l_layer(p, gadm, label="Swiss Cantons", parent=g1, index=1,
	          asSingleLayer=FALSE,
              color="", linecolor="red")
~~~

![](images/layer_map_cantons.png)

Finally, we label the canton layers accordingly

~~~
cantons <- gadm@data$NAME_1[gadm@plotOrder]

for (i in 1:length(m1)) {
    sapply(m1[[i]], function(l)l_layer_relabel(p, l, cantons[i]))
}
~~~


![](images/layer_map_labels_before.png)  ![](images/layer_map_labels_after.png) 


# Generics

`l_layer` is a generic function and you may add a method to layer a
visual representation for an object of a particular class.

~~~
methods('l_layer')
~~~

Here a short example for an object of class `foo`

~~~
newFoo <- function(x, y, ...) {
	r <- list(x=x, y=y, ...)
	class(r) <- 'foo'
	return(r)
}
~~~

Then the layer function is

~~~
l_layer.foo <- function(widget, x) {
	x$widget <- widget
	id <- do.call('l_layer_polygon', x)
	return(id)
}
~~~

And finally

~~~
p <- l_plot()

obj <- newFoo(x=c(1:6,6:2), y=c(3,1,0,0,1,3,3,5,6,6,5), color='yellow')

id <- l_layer(p, obj)

l_scaleto_world(p)
~~~


![](images/layer_generic.png) 


# countourLines \& heatImage \& rasterImage

We provide the functions `l_layer_contourLines`, `l_layer_heatImage`,
and `l_layer_rasterImage` that similar to the `R` functions
`contourLines`, `image`, and `rasterImage`, respectively. See the
examples for each function with the `examples` `R` function. 

For example:

~~~
kest <- with(iris, MASS::kde2d(Sepal.Width,Sepal.Length))
p <- with(iris, l_plot(Sepal.Width,Sepal.Length, color='black'))
l_layer_contourLines(p, kest)
l_layer_heatImage(p, kest) 
l_scaleto_world(p)  
~~~

![](images/layers_heatimage.png)


`l_layer_contourLines` creates a lines layer, and `l_layer_heatImage`,
and `l_layer_rasterImage` a rectangles layer.

</R>
