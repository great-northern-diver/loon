pkgname <- "loon"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "loon-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('loon')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("as.graph")
### * as.graph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as.graph
### Title: Convert a loongraph object to an object of class graph
### Aliases: as.graph

### ** Examples

library(graph)
g <- loongraph(letters[1:4], letters[1:3], letters[2:4], FALSE)
g1 <- as.graph(g) 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as.graph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("as.loongraph")
### * as.loongraph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as.loongraph
### Title: Convert a graph object to a loongraph object
### Aliases: as.loongraph

### ** Examples

library(graph)
graph_graph  = randomEGraph(LETTERS[1:15], edges=100)

loon_graph <- as.loongraph(graph_graph)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as.loongraph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("color_loon")
### * color_loon

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: color_loon
### Title: Create a palette with loon's color mapping
### Aliases: color_loon

### ** Examples


pal <- color_loon()
pal(letters[1:4]) 
pal(c('a','a','b','c'))
pal(c('green', 'yellow'))

# show color choices for different n's
library(grid)
grid.newpage()
pushViewport(plotViewport())
grid.rect()
n <- 2^(1:5)
pushViewport(dataViewport(xscale=c(0, max(n)+1), yscale=c(0, length(n)+1)))
grid.yaxis(at=c(1:length(n)), label=paste("n =", n))
for (i in rev(seq_along(n))) {
 cols <- pal(1:n[i])
 grid.points(x = 1:n[i], y = rep(i, n[i]), default.units = "native", pch=15, gp=gpar(col=cols))
}
grid.text("note the fist i colors are shared for each n" , y=unit(1,"npc")+unit(1, "line"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("color_loon", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("completegraph")
### * completegraph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: completegraph
### Title: Create a complete graph or digraph with a set of nodes
### Aliases: completegraph

### ** Examples

g <- loon::completegraph(letters[1:5])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("completegraph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("graphreduce")
### * graphreduce

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: graphreduce
### Title: Make each space in a node apprear only once
### Aliases: graphreduce

### ** Examples

G <- completegraph(nodes=LETTERS[1:4])
LG <- linegraph(G)

LLG <- linegraph(LG)

graphreduce(LLG)

## Not run: 
##D library(Rgraphviz)  
##D plot(graphreduce(LLG))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("graphreduce", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_Rlist2nestedTclList")
### * l_Rlist2nestedTclList

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_Rlist2nestedTclList
### Title: Convert an R list to a nested Tcl list
### Aliases: l_Rlist2nestedTclList

### ** Examples

x <- list(c(1,3,4), c(4,3,2,1), c(4,3,2,5,6))
l_Rlist2nestedTclList(x)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_Rlist2nestedTclList", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_aspect-set")
### * l_aspect-set

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_aspect<-
### Title: Set the aspect ratio of a plot
### Aliases: l_aspect<-

### ** Examples

p <- with(iris, l_plot(Sepal.Length ~ Sepal.Width, color=Species))

l_aspect(p)
l_aspect(p) <- 1



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_aspect-set", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_aspect")
### * l_aspect

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_aspect
### Title: Query the aspect ratio of a plot
### Aliases: l_aspect

### ** Examples

p <- with(iris, l_plot(Sepal.Length ~ Sepal.Width, color=Species))

l_aspect(p)
l_aspect(p) <- 1



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_aspect", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_bind_canvas")
### * l_bind_canvas

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_bind_canvas
### Title: Create a Canvas Binding
### Aliases: l_bind_canvas

### ** Examples

# binding for when plot is resized 
p <- l_plot(iris[,1:2], color=iris$Species)

printSize <- function(p) {
    size <- l_size(p)
    cat(paste('Size of widget ', p, ' is: ',
              size[1], 'x', size[2], ' pixels\n', sep=''))    
}

l_bind_canvas(p, event='<Configure>', function(W) {printSize(W)})

id <- l_bind_canvas_ids(p)
id

l_bind_canvas_get(p, id)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_bind_canvas", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_bind_canvas_get")
### * l_bind_canvas_get

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_bind_canvas_get
### Title: Get the event pattern and callback Tcl code of a canvas binding
### Aliases: l_bind_canvas_get

### ** Examples

# binding for when plot is resized 
p <- l_plot(iris[,1:2], color=iris$Species)

printSize <- function(p) {
    size <- l_size(p)
    cat(paste('Size of widget ', p, ' is: ',
              size[1], 'x', size[2], ' pixels\n', sep=''))    
}

l_bind_canvas(p, event='<Configure>', function(W) {printSize(W)})

id <- l_bind_canvas_ids(p)
id

l_bind_canvas_get(p, id)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_bind_canvas_get", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_bind_canvas_ids")
### * l_bind_canvas_ids

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_bind_canvas_ids
### Title: List canvas binding ids
### Aliases: l_bind_canvas_ids

### ** Examples

# binding for when plot is resized 
p <- l_plot(iris[,1:2], color=iris$Species)

printSize <- function(p) {
    size <- l_size(p)
    cat(paste('Size of widget ', p, ' is: ',
              size[1], 'x', size[2], ' pixels\n', sep=''))    
}

l_bind_canvas(p, event='<Configure>', function(W) {printSize(W)})

id <- l_bind_canvas_ids(p)
id

l_bind_canvas_get(p, id)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_bind_canvas_ids", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_cget")
### * l_cget

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_cget
### Title: Query a Plot State
### Aliases: l_cget [.loon

### ** Examples

p <- l_plot(iris, color = iris$Species)
l_cget(p, "color")
p['selected']



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_cget", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_configure")
### * l_configure

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_configure
### Title: Modify one or multiple plot states
### Aliases: l_configure [<-.loon

### ** Examples

p <- l_plot(iris, color = iris$Species)
l_configure(p, color='red')
p['size'] <- ifelse(iris$Species == "versicolor", 2, 8)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_configure", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_context_add_slicing2d")
### * l_context_add_slicing2d

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_context_add_slicing2d
### Title: Create a slicind2d navigator context
### Aliases: l_context_add_slicing2d

### ** Examples

names(oliveAcids) <- c('p','p1','s','o','l','l1','a','e')
nodes <- apply(combn(names(oliveAcids),2),2,
              function(x)paste(x, collapse=':'))
G <- completegraph(nodes)
g <- l_graph(G)
nav <- l_navigator_add(g)
con <- l_context_add_slicing2d(nav, data=oliveAcids)

# symmetric range proportion around nav['proportion']
con['proportion'] <- 0.2

con['conditioning4d'] <- "union"
con['conditioning4d'] <- "intersection"




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_context_add_slicing2d", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_create_handle")
### * l_create_handle

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_create_handle
### Title: Create a loon object handle
### Aliases: l_create_handle

### ** Examples


# plot handle
p <- l_plot(x=1:3, y=1:3)
p_new <- l_create_handle(unclass(p))
p_new['showScales']

# glyph handle
gl <- l_glyph_add_text(p, text=LETTERS[1:3])
gl_new <- l_create_handle(c(as.vector(p), as.vector(gl)))
gl_new['text']

# layer handle
l <- l_layer_rectangle(p, x=c(1,3), y=c(1,3), color='yellow', index='end')
l_new <- l_create_handle(c(as.vector(p), as.vector(l)))
l_new['color']

# navigator handle
g <- l_graph(linegraph(completegraph(LETTERS[1:3])))
nav <- l_navigator_add(g)
nav_new <- l_create_handle(c(as.vector(g), as.vector(nav)))
nav_new['from']

# context handle
con <- l_context_add_context2d(nav)
con_new <- l_create_handle(c(as.vector(g), as.vector(nav), as.vector(con)))
con_new['separator']



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_create_handle", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_currentindex")
### * l_currentindex

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_currentindex
### Title: Get layer-relative index of the item below the mouse cursor
### Aliases: l_currentindex

### ** Examples

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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_currentindex", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_currenttags")
### * l_currenttags

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_currenttags
### Title: Get tags of the item below the mouse cursor
### Aliases: l_currenttags

### ** Examples

printTags <- function(W) {
    print(l_currenttags(W))
}

p <- l_plot(x=1:3, y=1:3, title='Query Visual Item Tags')

l_bind_item(p, 'all', '<ButtonPress>', function(W)printTags(W))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_currenttags", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_glyph_add")
### * l_glyph_add

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_glyph_add
### Title: Add non-primitive glyphs to a scatterplot or graph display
### Aliases: l_glyph_add

### ** Examples

# Simple Example with Text Glyphs
p <- with(olive, l_plot(stearic, eicosenoic, color=Region))
g <- l_glyph_add_text(p, text=olive$Area, label="Area")
p['glyph'] <- g

## Not run: 
##D demo("l_glyphs", package="loon")
## End(Not run)

# create a plot that demonstrates the primitive glyphs and the text glyphs
p <- l_plot(x=1:15, y=rep(0,15), size=10, showLabels=FALSE)
text_glyph <- l_glyph_add_text(p, text=letters [1:15])
p['glyph'] <- c(
    'circle', 'ocircle', 'ccircle',
    'square', 'osquare' , 'csquare',
    'triangle', 'otriangle', 'ctriangle',
    'diamond', 'odiamond', 'cdiamond',
    rep(text_glyph, 3)
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_glyph_add", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_glyph_add_image")
### * l_glyph_add_image

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_glyph_add_image
### Title: Add an image glyphs
### Aliases: l_glyph_add_image

### ** Examples

## Not run: 
##D p <- with(olive, l_plot(palmitic ~ stearic, color = Region))
##D img_paths <- list.files(file.path(find.package(package = 'loon'), "images"), full.names = TRUE)
##D imgs <- setNames(l_image_import_files(img_paths),
##D                  tools::file_path_sans_ext(basename(img_paths)))
##D i <- pmatch(gsub("^[[:alpha:]]+-","", olive$Area), names(imgs), duplicates.ok = TRUE)
##D 
##D g <- l_glyph_add_image(p, imgs[i], label="Flags")
##D p['glyph'] <- g
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_glyph_add_image", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_glyph_add_pointrange")
### * l_glyph_add_pointrange

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_glyph_add_pointrange
### Title: Add a Pointrange Glyph
### Aliases: l_glyph_add_pointrange

### ** Examples

p <- l_plot(x = 1:3, color = c('red', 'blue', 'green'), showScales=TRUE)
g <- l_glyph_add_pointrange(p, ymin=(1:3)-(1:3)/5, ymax=(1:3)+(1:3)/5)
p['glyph'] <- g



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_glyph_add_pointrange", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_glyph_add_polygon")
### * l_glyph_add_polygon

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_glyph_add_polygon
### Title: Add a Polygon Glyph
### Aliases: l_glyph_add_polygon

### ** Examples

x_star <- 
    c(-0.000864304235090734, 0.292999135695765, 0.949870354364736, 
      0.474503025064823, 0.586862575626621, -0.000864304235090734, 
      -0.586430423509075, -0.474070872947277, -0.949438202247191,
      -0.29256698357822)
y_star <-
    c(-1, -0.403630077787381, -0.308556611927398, 0.153846153846154, 
      0.808556611927398, 0.499567847882455, 0.808556611927398,
      0.153846153846154, -0.308556611927398, -0.403630077787381)
x_cross <- 
    c(-0.258931143762604, -0.258931143762604, -0.950374531835206, 
      -0.950374531835206, -0.258931143762604, -0.258931143762604,
      0.259651397291847, 0.259651397291847, 0.948934024776722,
      0.948934024776722, 0.259651397291847, 0.259651397291847)
y_cross <-
    c(-0.950374531835206, -0.258931143762604, -0.258931143762604, 
      0.259651397291847, 0.259651397291847, 0.948934024776722,
      0.948934024776722, 0.259651397291847, 0.259651397291847,
      -0.258931143762604, -0.258931143762604, -0.950374531835206)
x_hexagon <-
    c(0.773552290406223, 0, -0.773552290406223, -0.773552290406223, 
      0, 0.773552290406223)
y_hexagon <- 
    c(0.446917314894843, 0.894194756554307, 0.446917314894843,
      -0.447637568424085, -0.892754249495822, -0.447637568424085)

p <- l_plot(1:3, 1:3)

gl <- l_glyph_add_polygon(p, x = list(x_star, x_cross, x_hexagon),
                          y = list(y_star, y_cross, y_hexagon))

p['glyph'] <- gl

gl['showArea'] <- FALSE



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_glyph_add_polygon", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_glyph_add_serialaxes")
### * l_glyph_add_serialaxes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_glyph_add_serialaxes
### Title: Add a Serialaxes Glyph
### Aliases: l_glyph_add_serialaxes

### ** Examples

p <- with(olive, l_plot(oleic, stearic, color=Area))
gs <- l_glyph_add_serialaxes(p, data=olive[,-c(1,2)], showArea=FALSE)
p['glyph'] <- gs



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_glyph_add_serialaxes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_glyph_add_text")
### * l_glyph_add_text

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_glyph_add_text
### Title: Add a Text Glyph
### Aliases: l_glyph_add_text

### ** Examples

p <- l_plot(iris, color = iris$Species)
g <- l_glyph_add_text(p, iris$Species, "test_label")
p['glyph'] <- g



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_glyph_add_text", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_glyph_relabel")
### * l_glyph_relabel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_glyph_relabel
### Title: Relabel Glyph
### Aliases: l_glyph_relabel

### ** Examples

p <- l_plot(iris, color = iris$Species)
g <- l_glyph_add_text(p, iris$Species, "test_label")
p['glyph'] <- g
l_glyph_relabel(p, g, "Species")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_glyph_relabel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_glyphs_inspector")
### * l_glyphs_inspector

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_glyphs_inspector
### Title: Create a Glyphs Inspector
### Aliases: l_glyphs_inspector

### ** Examples

i <- l_glyphs_inspector()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_glyphs_inspector", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_glyphs_inspector_image")
### * l_glyphs_inspector_image

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_glyphs_inspector_image
### Title: Create a Image Glyph Inspector
### Aliases: l_glyphs_inspector_image

### ** Examples

i <- l_glyphs_inspector_image()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_glyphs_inspector_image", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_glyphs_inspector_pointrange")
### * l_glyphs_inspector_pointrange

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_glyphs_inspector_pointrange
### Title: Create a Pointrange Glyph Inspector
### Aliases: l_glyphs_inspector_pointrange

### ** Examples

i <- l_glyphs_inspector_pointrange()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_glyphs_inspector_pointrange", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_glyphs_inspector_serialaxes")
### * l_glyphs_inspector_serialaxes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_glyphs_inspector_serialaxes
### Title: Create a Serialaxes Glyph Inspector
### Aliases: l_glyphs_inspector_serialaxes

### ** Examples

i <- l_glyphs_inspector_serialaxes()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_glyphs_inspector_serialaxes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_glyphs_inspector_text")
### * l_glyphs_inspector_text

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_glyphs_inspector_text
### Title: Create a Text Glyph Inspector
### Aliases: l_glyphs_inspector_text

### ** Examples

i <- l_glyphs_inspector_text()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_glyphs_inspector_text", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_graph_inspector")
### * l_graph_inspector

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_graph_inspector
### Title: Create a Graph Inspector
### Aliases: l_graph_inspector

### ** Examples

i <- l_graph_inspector()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_graph_inspector", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_graph_inspector_analysis")
### * l_graph_inspector_analysis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_graph_inspector_analysis
### Title: Create a Graph Analysis Inspector
### Aliases: l_graph_inspector_analysis

### ** Examples

i <- l_graph_inspector_analysis()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_graph_inspector_analysis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_graph_inspector_navigators")
### * l_graph_inspector_navigators

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_graph_inspector_navigators
### Title: Create a Graph Navigator Inspector
### Aliases: l_graph_inspector_navigators

### ** Examples

i <- l_graph_inspector_navigators()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_graph_inspector_navigators", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_help")
### * l_help

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_help
### Title: Open a browser with loon's documentation webpage
### Aliases: l_help

### ** Examples


## Not run: 
##D l_help()
##D l_help("learn_R_display_hist")
##D l_help("learn_R_bind")
##D # jump to a section
##D l_help("learn_R_bind.html#list-reorder-delete-bindings")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_help", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_hexcolor")
### * l_hexcolor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_hexcolor
### Title: Convert color names to their 12 digit hexadecimal color
###   representation
### Aliases: l_hexcolor

### ** Examples

p <- l_plot(1:2)
p['color'] <- 'red'
p['color']

l_hexcolor('red')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_hexcolor", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_hist")
### * l_hist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_hist
### Title: Create an Interactive Histogram
### Aliases: l_hist

### ** Examples

h <- l_hist(iris$Sepal.Length, color=iris$Species)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_hist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_hist_inspector")
### * l_hist_inspector

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_hist_inspector
### Title: Create a Histogram Inspector
### Aliases: l_hist_inspector

### ** Examples

i <- l_hist_inspector()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_hist_inspector", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_hist_inspector_analysis")
### * l_hist_inspector_analysis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_hist_inspector_analysis
### Title: Create a Histogram Analysis Inspector
### Aliases: l_hist_inspector_analysis

### ** Examples

i <- l_hist_inspector_analysis()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_hist_inspector_analysis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_image_import_array")
### * l_image_import_array

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_image_import_array
### Title: Import Greyscale Images as Tcl images from an Array
### Aliases: l_image_import_array

### ** Examples


## Not run: 
##D # see
##D demo("l_ng_images_frey_LLE")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_image_import_array", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_imageviewer")
### * l_imageviewer

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_imageviewer
### Title: Display Tcl Images in a Simple Image Viewer
### Aliases: l_imageviewer

### ** Examples


img2 <- tkimage.create('photo', width=200, height=150)
tcl(img2, 'put', 'yellow', '-to', 0, 0, 199, 149)
tcl(img2, 'put', 'green', '-to', 40, 20, 130, 40)
img3 <- tkimage.create('photo', width=500, height=100)
tcl(img3, 'put', 'orange', '-to', 0, 0, 499, 99)
tcl(img3, 'put', 'green', '-to', 40, 80, 350, 95)

l_imageviewer(c(tclvalue(img2), tclvalue(img3)))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_imageviewer", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_info_states")
### * l_info_states

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_info_states
### Title: Retrieve Information about the States of a Loon Widget
### Aliases: l_info_states

### ** Examples

p <- l_plot(iris, linkingGroup="iris")
i <- l_info_states(p)
names(i)
i$selectBy

l <- l_layer_rectangle(p, x=range(iris[,1]), y=range(iris[,2]), color="")
l_info_states(l)


h <- l_hist(iris$Sepal.Length, linkingGroup="iris")
l_info_states(h)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_info_states", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer.Line")
### * l_layer.Line

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer.Line
### Title: Layer line in Line object
### Aliases: l_layer.Line

### ** Examples

library(sp)
library(rworldmap)

world <- getMap(resolution = "coarse")
p <- l_plot()
lmap <- l_layer(p, world, asSingleLayer=TRUE)
l_scaleto_world(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer.Line", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer.Lines")
### * l_layer.Lines

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer.Lines
### Title: Layer lines in Lines object
### Aliases: l_layer.Lines

### ** Examples

library(sp)
library(rworldmap)

world <- getMap(resolution = "coarse")
p <- l_plot()
lmap <- l_layer(p, world, asSingleLayer=TRUE)
l_scaleto_world(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer.Lines", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer.Polygon")
### * l_layer.Polygon

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer.Polygon
### Title: Layer polygon in Polygon object
### Aliases: l_layer.Polygon

### ** Examples

library(sp)
library(rworldmap)

world <- getMap(resolution = "coarse")
p <- l_plot()
lmap <- l_layer(p, world, asSingleLayer=TRUE)
l_scaleto_world(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer.Polygon", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer.Polygons")
### * l_layer.Polygons

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer.Polygons
### Title: Layer polygons in Polygons object
### Aliases: l_layer.Polygons

### ** Examples

library(sp)
library(rworldmap)

world <- getMap(resolution = "coarse")
p <- l_plot()
lmap <- l_layer(p, world, asSingleLayer=TRUE)
l_scaleto_world(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer.Polygons", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer")
### * l_layer

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer
### Title: Loon layers
### Aliases: l_layer

### ** Examples

# l_layer is a generic method
newFoo <- function(x, y, ...) {
  r <- list(x=x, y=y, ...)
  class(r) <- 'foo'
  return(r)
}

l_layer.foo <- function(widget, x) {
    x$widget <- widget
    id <- do.call('l_layer_polygon', x)
    return(id)
}

p <- l_plot()

obj <- newFoo(x=c(1:6,6:2), y=c(3,1,0,0,1,3,3,5,6,6,5), color='yellow')

id <- l_layer(p, obj)

l_scaleto_world(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer.SpatialLines")
### * l_layer.SpatialLines

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer.SpatialLines
### Title: Layer lines in SpatialLines object
### Aliases: l_layer.SpatialLines

### ** Examples

library(sp)
library(rworldmap)

world <- getMap(resolution = "coarse")
p <- l_plot()
lmap <- l_layer(p, world, asSingleLayer=TRUE)
l_scaleto_world(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer.SpatialLines", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer.SpatialLinesDataFrame")
### * l_layer.SpatialLinesDataFrame

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer.SpatialLinesDataFrame
### Title: Layer lines in SpatialLinesDataFrame object
### Aliases: l_layer.SpatialLinesDataFrame

### ** Examples

library(sp)
library(rworldmap)

world <- getMap(resolution = "coarse")
p <- l_plot()
lmap <- l_layer(p, world, asSingleLayer=TRUE)
l_scaleto_world(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer.SpatialLinesDataFrame", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer.SpatialPoints")
### * l_layer.SpatialPoints

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer.SpatialPoints
### Title: Layer points in SpatialPoints object
### Aliases: l_layer.SpatialPoints

### ** Examples

library(sp)
library(rworldmap)

world <- getMap(resolution = "coarse")
p <- l_plot()
lmap <- l_layer(p, world, asSingleLayer=TRUE)
l_scaleto_world(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer.SpatialPoints", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer.SpatialPointsDataFrame")
### * l_layer.SpatialPointsDataFrame

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer.SpatialPointsDataFrame
### Title: Layer points in SpatialPointsDataFrame object
### Aliases: l_layer.SpatialPointsDataFrame

### ** Examples

library(sp)
library(rworldmap)

world <- getMap(resolution = "coarse")
p <- l_plot()
lmap <- l_layer(p, world, asSingleLayer=TRUE)
l_scaleto_world(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer.SpatialPointsDataFrame", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer.SpatialPolygons")
### * l_layer.SpatialPolygons

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer.SpatialPolygons
### Title: Layer polygons in SpatialPolygons object
### Aliases: l_layer.SpatialPolygons

### ** Examples

library(sp)
library(rworldmap)

world <- getMap(resolution = "coarse")
p <- l_plot()
lmap <- l_layer(p, world, asSingleLayer=TRUE)
l_scaleto_world(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer.SpatialPolygons", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer.SpatialPolygonsDataFrame")
### * l_layer.SpatialPolygonsDataFrame

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer.SpatialPolygonsDataFrame
### Title: Layer polygons in SpatialPolygonDataFrame
### Aliases: l_layer.SpatialPolygonsDataFrame

### ** Examples

library(sp)
library(rworldmap)

world <- getMap(resolution = "coarse")
p <- l_plot()
lmap <- l_layer(p, world, asSingleLayer=TRUE)
l_scaleto_world(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer.SpatialPolygonsDataFrame", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer.density")
### * l_layer.density

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer.density
### Title: Layer Method for Kernel Density Estimation
### Aliases: l_layer.density

### ** Examples

 
d <- density(faithful$eruptions, bw = "sj")
h <- l_hist(x = faithful$eruptions, yshows="density")
l <- l_layer.density(h, d, color="steelblue", linewidth=3)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer.density", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer.map")
### * l_layer.map

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer.map
### Title: Add a Map of class map as Drawings to Loon plot
### Aliases: l_layer.map

### ** Examples

library(maps)
canada <- map("world",  "Canada", fill=TRUE, plot=FALSE)
p <- l_plot()
l_map <- l_layer(p, canada, asSingleLayer=TRUE)
l_map['color'] <- ifelse(grepl("lake", canada$names, TRUE), "lightblue", "")
l_scaleto_layer(p, l_map)
l_map['active'] <- FALSE
l_map['active'] <- TRUE
l_map['tag']



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer.map", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_bbox")
### * l_layer_bbox

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_bbox
### Title: Get the bounding box of a layer.
### Aliases: l_layer_bbox

### ** Examples

p <- with(iris, l_plot(Sepal.Length ~ Sepal.Width, color=Species))
l_layer_bbox(p, layer='model')

l <- l_layer_rectangle(p, x=0:1, y=30:31)
l_layer_bbox(p, l)

l_layer_bbox(p, 'root')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_bbox", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_contourLines")
### * l_layer_contourLines

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_contourLines
### Title: Layer Contour Lines
### Aliases: l_layer_contourLines

### ** Examples

p <- l_plot()
x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
lcl <- l_layer_contourLines(p, x, y, volcano)
l_scaleto_world(p)

library(MASS)
p1 <- with(iris, l_plot(Sepal.Length~Sepal.Width, color=Species))
lcl <- with(iris, l_layer_contourLines(p1, MASS::kde2d(Sepal.Width,Sepal.Length)))

p2 <- with(iris, l_plot(Sepal.Length~Sepal.Width, color=Species))
layers <- sapply(split(cbind(iris, color=p2['color']), iris$Species), function(dat) {
     kest <- with(dat, MASS::kde2d(Sepal.Width,Sepal.Length))
     l_layer_contourLines(p2, kest, color=as.character(dat$color[1]), linewidth=2,
          label=paste0(as.character(dat$Species[1]), " contours"))
})





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_contourLines", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_delete")
### * l_layer_delete

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_delete
### Title: Delete a layer
### Aliases: l_layer_delete

### ** Examples

p <- l_plot()
l1 <- l_layer_rectangle(p, x = 0:1, y = 0:1, color='red')
l_layer_delete(l1)

l2 <- l_layer_rectangle(p, x = 0:1, y = 0:1, color='yellow')
l_layer_delete(p,l2)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_delete", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_demote")
### * l_layer_demote

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_demote
### Title: Moves the layer to be a child of its right group layer sibling
### Aliases: l_layer_demote

### ** Examples

p <- l_plot()

g1 <- l_layer_group(p)
g2 <- l_layer_group(p, parent=g1)
l1 <- l_layer_oval(p, x=0:1, y=0:1)

l_layer_printTree(p) 
l_layer_demote(p, l1)
l_layer_printTree(p)
l_layer_demote(p, l1)
l_layer_printTree(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_demote", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_expunge")
### * l_layer_expunge

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_expunge
### Title: Delete a layer and all its descendants
### Aliases: l_layer_expunge

### ** Examples


p <- l_plot()
g <- l_layer_group(p)
l1 <- l_layer_rectangle(p, x=0:1, y=0:1, parent=g, color="", linecolor="orange", linewidth=2)
l2 <- l_layer_line(p, x=c(0,.5,1), y=c(0,1,0), parent=g, color="blue")

l_layer_expunge(p, g)

# or l_layer_expunge(g)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_expunge", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_getChildren")
### * l_layer_getChildren

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_getChildren
### Title: Get children of a group layer
### Aliases: l_layer_getChildren

### ** Examples

p <- l_plot()

g <- l_layer_group(p)
l1 <- l_layer_rectangle(p, x=0:1, y=0:1, parent=g)
l2 <- l_layer_oval(p, x=0:1, y=0:1, color='thistle', parent=g)

l_layer_getChildren(p, g)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_getChildren", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_getLabel")
### * l_layer_getLabel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_getLabel
### Title: Get layer label.
### Aliases: l_layer_getLabel

### ** Examples

p <- l_plot()
l1 <- l_layer_rectangle(p, x=0:1, y=0:1, label="a rectangle")
l_layer_getLabel(p, 'model')
l_layer_getLabel(p, l1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_getLabel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_getParent")
### * l_layer_getParent

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_getParent
### Title: Get parent layer id of a layer
### Aliases: l_layer_getParent

### ** Examples

p <- with(iris, l_plot(Sepal.Length ~ Sepal.Width, color=Species))

l_layer_getParent(p, 'model')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_getParent", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_getType")
### * l_layer_getType

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_getType
### Title: Get layer type
### Aliases: l_layer_getType

### ** Examples

p <- l_plot()
l <- l_layer_rectangle(p, x=0:1, y=0:1)
l_layer_getType(p, l)
l_layer_getType(p, 'model')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_getType", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_group")
### * l_layer_group

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_group
### Title: layer a group node
### Aliases: l_layer_group

### ** Examples

p <- l_plot(x=c(1,10,1.5,7,4.3,9,5,2,8),
             y=c(1,10,7,3,4,3.3,8,3,4),
             title="Demo Layers")

id.g <- l_layer_group(p, "A Layer Group")
id.pts <- l_layer_points(p, x=c(3,6), y=c(4,7), color="red", parent=id.g)
l_scaleto_layer(p, id.pts) 
l_configure(id.pts, x=c(-5,5,12), y=c(-2,-5,18), color="lightgray")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_group", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_groupVisibility")
### * l_layer_groupVisibility

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_groupVisibility
### Title: Queries visibility status of decendants
### Aliases: l_layer_groupVisibility

### ** Examples


p <- l_plot()

g <- l_layer_group(p)
l1 <- l_layer_rectangle(p, x=0:1, y=0:1, parent=g)
l2 <- l_layer_oval(p, x=0:1, y=0:1, parent=g)

l_layer_groupVisibility(p, g)
l_layer_hide(p, l2)
l_layer_groupVisibility(p, g)
l_layer_hide(p, l1)
l_layer_groupVisibility(p, g)
l_layer_hide(p, g)
l_layer_groupVisibility(p, g)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_groupVisibility", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_heatImage")
### * l_layer_heatImage

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_heatImage
### Title: Display a Heat Image
### Aliases: l_layer_heatImage

### ** Examples

library(MASS)
kest <- with(iris, MASS::kde2d(Sepal.Width,Sepal.Length))
image(kest)
contour(kest, add=TRUE)

p <- l_plot()
lcl <- l_layer_contourLines(p, kest, label='contour lines')
limg <- l_layer_heatImage(p, kest, label='heatmap') 
l_scaleto_world(p)

# from examples(image)
x <- y <- seq(-4*pi, 4*pi, len = 27)
r <- sqrt(outer(x^2, y^2, "+"))
p1 <- l_plot()
l_layer_heatImage(p1, z = z <- cos(r^2)*exp(-r/6), col  = gray((0:32)/32))
l_scaleto_world(p1)

image(z = z <- cos(r^2)*exp(-r/6), col  = gray((0:32)/32))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_heatImage", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_hide")
### * l_layer_hide

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_hide
### Title: Hide a Layer
### Aliases: l_layer_hide

### ** Examples

p <- l_plot()

l <- l_layer_rectangle(p, x=0:1, y=0:1, color="steelblue")
l_layer_hide(p, l)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_hide", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_ids")
### * l_layer_ids

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_ids
### Title: List ids of layers in Plot
### Aliases: l_layer_ids

### ** Examples

set.seed(500)
x <- rnorm(30)
y <- 4 + 3*x + rnorm(30)
fit <- lm(y~x)
xseq <- seq(min(x)-1, max(x)+1, length.out = 50)
fit_line <- predict(fit, data.frame(x=range(xseq)))
ci <- predict(fit, data.frame(x=xseq), 
              interval="confidence", level=0.95)
pi <- predict(fit, data.frame(x=xseq),
              interval="prediction", level=0.95)


p <- l_plot(y~x, color='black', showScales=TRUE, showGuides=TRUE)
gLayer <- l_layer_group(
    p, label="simple linear regression",
    parent="root", index="end"
)
fitLayer <- l_layer_line(
    p, x=range(xseq), y=fit_line, color="#04327F",
    linewidth=4, label="fit", parent=gLayer
)
ciLayer <- l_layer_polygon(
    p,
    x = c(xseq, rev(xseq)), 
    y = c(ci[,'lwr'], rev(ci[,'upr'])),
    color = "#96BDFF", linecolor="",
    label = "95 % confidence interval",
    parent = gLayer, index='end'
)
piLayer <- l_layer_polygon(
    p,
    x = c(xseq, rev(xseq)), 
    y = c(pi[,'lwr'], rev(pi[,'upr'])),
    color = "#E2EDFF", linecolor="",
    label = "95 % prediction interval",
    parent = gLayer, index='end'
)

l_info_states(piLayer)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_ids", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_isVisible")
### * l_layer_isVisible

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_isVisible
### Title: Return visibility flag of layer
### Aliases: l_layer_isVisible

### ** Examples

p <- l_plot()
l <- l_layer_rectangle(p, x=0:1, y=0:1)
l_layer_isVisible(p, l)
l_layer_hide(p, l)
l_layer_isVisible(p, l)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_isVisible", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_line")
### * l_layer_line

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_line
### Title: Layer a line
### Aliases: l_layer_line

### ** Examples


p <- l_plot()
l <- l_layer_line(p, x=c(1,2,3,4), y=c(1,3,2,4), color='red', linewidth=2)
l_scaleto_world(p)

# object
p <- l_plot()
l <- l_layer_line(p, x=nhtemp)
l_scaleto_layer(l)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_line", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_lines")
### * l_layer_lines

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_lines
### Title: Layer a lines
### Aliases: l_layer_lines

### ** Examples

s <- Filter(function(df)nrow(df) > 1, split(UsAndThem, UsAndThem$Country))
sUaT <- Map(function(country){country[order(country$Year),]} , s)
xcoords <- Map(function(x)x$Year, sUaT)
ycoords <- Map(function(x)x$LifeExpectancy, sUaT)
region <- sapply(sUaT, function(x)as.character(x$Geographic.Region[1]))

p <- l_plot(showItemLabels=TRUE)
l <- l_layer_lines(p, xcoords, ycoords, itemLabel=names(sUaT), color=region)
l_scaleto_layer(l)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_lines", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_lower")
### * l_layer_lower

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_lower
### Title: Switch the layer place with its sibling to the right
### Aliases: l_layer_lower

### ** Examples

p <- l_plot()

l1 <- l_layer_rectangle(p, x=0:1, y=0:1)
l2 <- l_layer_oval(p, x=0:1, y=0:1, color='thistle')

l_aspect(p) <- 1

l_layer_lower(p, l2)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_lower", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_move")
### * l_layer_move

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_move
### Title: Move a layer
### Aliases: l_layer_move

### ** Examples

p <- l_plot()

l <- l_layer_rectangle(p, x=0:1, y=0:1, color="steelblue")
g <- l_layer_group(p)
l_layer_printTree(p)

l_layer_move(l, parent=g)
l_layer_printTree(p)

l_layer_move(p, 'model', parent=g)
l_layer_printTree(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_move", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_oval")
### * l_layer_oval

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_oval
### Title: Layer a oval
### Aliases: l_layer_oval

### ** Examples

p <- l_plot()
l <- l_layer_oval(p, c(1,5), c(2,12), color='steelblue')
l_configure(p, panX=0, panY=0, deltaX=20, deltaY=20)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_oval", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_polygon")
### * l_layer_polygon

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_polygon
### Title: Layer a polygon
### Aliases: l_layer_polygon

### ** Examples

set.seed(500)
x <- rnorm(30)
y <- 4 + 3*x + rnorm(30)
fit <- lm(y~x)
xseq <- seq(min(x)-1, max(x)+1, length.out = 50)
fit_line <- predict(fit, data.frame(x=range(xseq)))
ci <- predict(fit, data.frame(x=xseq), 
              interval="confidence", level=0.95)
pi <- predict(fit, data.frame(x=xseq),
              interval="prediction", level=0.95)


p <- l_plot(y~x, color='black', showScales=TRUE, showGuides=TRUE)
gLayer <- l_layer_group(
    p, label="simple linear regression",
    parent="root", index="end"
)
fitLayer <- l_layer_line(
    p, x=range(xseq), y=fit_line, color="#04327F",
    linewidth=4, label="fit", parent=gLayer
)
ciLayer <- l_layer_polygon(
    p,
    x = c(xseq, rev(xseq)), 
    y = c(ci[,'lwr'], rev(ci[,'upr'])),
    color = "#96BDFF", linecolor="",
    label = "95 % confidence interval",
    parent = gLayer, index='end'
)
piLayer <- l_layer_polygon(
    p,
    x = c(xseq, rev(xseq)), 
    y = c(pi[,'lwr'], rev(pi[,'upr'])),
    color = "#E2EDFF", linecolor="",
    label = "95 % prediction interval",
    parent = gLayer, index='end'
)

l_info_states(piLayer)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_polygon", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_polygons")
### * l_layer_polygons

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_polygons
### Title: Layer a polygons
### Aliases: l_layer_polygons

### ** Examples


p <- l_plot()

l <- l_layer_polygons(
     p,
     x = list(c(1,2,1.5), c(3,4,6,5,2), c(1,3,5,3)),
     y = list(c(1,1,2), c(1,1.5,1,4,2), c(3,5,6,4)),
     color = c('red', 'green', 'blue'),
     linecolor = ""
)
l_scaleto_world(p)

l_info_states(l, "color")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_polygons", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_printTree")
### * l_layer_printTree

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_printTree
### Title: Print the layer tree
### Aliases: l_layer_printTree

### ** Examples

p <- l_plot()
l_layer_rectangle(p, x=0:1, y=0:1)
g <- l_layer_group(p)
l_layer_oval(p, x=0:1, y=0:1, parent=g)
l_layer_line(p, x=0:1, y=0:1, parent=g)
l_layer_printTree(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_printTree", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_promote")
### * l_layer_promote

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_promote
### Title: Moves the layer up to be a left sibling of its parent
### Aliases: l_layer_promote

### ** Examples

p <- l_plot()

g1 <- l_layer_group(p)
g2 <- l_layer_group(p, parent=g1)
l1 <- l_layer_oval(p, x=0:1, y=0:1, parent=g2)

l_layer_printTree(p)
l_layer_promote(p, l1)
l_layer_printTree(p)
l_layer_promote(p, l1)
l_layer_printTree(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_promote", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_raise")
### * l_layer_raise

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_raise
### Title: Switch the layer place with its sibling to the left
### Aliases: l_layer_raise

### ** Examples

p <- l_plot()

l1 <- l_layer_rectangle(p, x=0:1, y=0:1)
l2 <- l_layer_oval(p, x=0:1, y=0:1, color='thistle')

l_aspect(p) <- 1

l_layer_raise(p, l1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_raise", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_rasterImage")
### * l_layer_rasterImage

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_rasterImage
### Title: Layer a Raster Image
### Aliases: l_layer_rasterImage

### ** Examples

plot(1,1, xlim = c(0,1), ylim=c(0,1))
mat <- matrix(c(0,0,0,0, 1,1), ncol=2)
rasterImage(mat, 0,0,1,1, interpolate = FALSE)

p <- l_plot()
l_layer_rasterImage(p, mat, 0,0,1,1)
l_scaleto_world(p)

# from examples(rasterImage)

# set up the plot region:
op <- par(bg = "thistle")
plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "")
image <- as.raster(matrix(0:1, ncol = 5, nrow = 3))
rasterImage(image, 100, 300, 150, 350, interpolate = FALSE)
rasterImage(image, 100, 400, 150, 450)
rasterImage(image, 200, 300, 200 + 10, 300 + 10,
           interpolate = FALSE)
           
p <- l_plot(showScales=TRUE, background="thistle", useLoonInspector=FALSE)
l_layer_rasterImage(p, image, 100, 300, 150, 350, interpolate = FALSE)
l_layer_rasterImage(p, image, 100, 400, 150, 450)
l_layer_rasterImage(p, image, 200, 300, 200 + 10, 300 + 10,
   interpolate = FALSE)
l_scaleto_world(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_rasterImage", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("l_layer_rectangle")
### * l_layer_rectangle

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_rectangle
### Title: Layer a rectangle
### Aliases: l_layer_rectangle

### ** Examples

p <- l_plot()
l <- l_layer_rectangle(p, x=c(2,3), y=c(1,10), color='steelblue')
l_scaleto_layer(l)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_rectangle", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_rectangles")
### * l_layer_rectangles

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_rectangles
### Title: Layer a rectangles
### Aliases: l_layer_rectangles

### ** Examples


p <- l_plot()

l <- l_layer_rectangles(
     p,
     x = list(c(0,1), c(1,2), c(2,3), c(5,6)),
     y = list(c(0,1), c(1,2), c(0,1), c(3,4)),
     color = c('red', 'blue', 'green', 'orange'),
     linecolor = "black"
)
l_scaleto_world(p)

l_info_states(l)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_rectangles", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_relabel")
### * l_layer_relabel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_relabel
### Title: Change layer label
### Aliases: l_layer_relabel

### ** Examples

p <- l_plot()

l <- l_layer_rectangle(p, x=0:1, y=0:1, label="A rectangle")
l_layer_getLabel(p, l)

l_layer_relabel(p, l, label="A relabelled rectangle")
l_layer_getLabel(p, l)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_relabel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_show")
### * l_layer_show

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_show
### Title: Show or unhide a Layer
### Aliases: l_layer_show

### ** Examples

p <- l_plot()

l <- l_layer_rectangle(p, x=0:1, y=0:1, color="steelblue")
l_layer_hide(p, l)

l_layer_show(p, l)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_show", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_text")
### * l_layer_text

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_text
### Title: Layer a text
### Aliases: l_layer_text

### ** Examples

p <- l_plot()
l <- l_layer_text(p, 0, 0, "Hello World")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_text", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layer_texts")
### * l_layer_texts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layer_texts
### Title: Layer a texts
### Aliases: l_layer_texts

### ** Examples

p <- l_plot()
l <- l_layer_texts(p, x=1:3, y=3:1, text=c("This is", "a", "test"), size=20)
l_scaleto_world(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layer_texts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_layers_inspector")
### * l_layers_inspector

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_layers_inspector
### Title: Create a Layers Inspector
### Aliases: l_layers_inspector

### ** Examples

i <- l_layers_inspector()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_layers_inspector", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_loon_inspector")
### * l_loon_inspector

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_loon_inspector
### Title: Create a loon linspector
### Aliases: l_loon_inspector

### ** Examples

i <- l_loon_inspector()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_loon_inspector", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_navgraph")
### * l_navgraph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_navgraph
### Title: Explore a dataset with the canonical 2d navigation graph setting
### Aliases: l_navgraph

### ** Examples

ng <- l_navgraph(oliveAcids, color=olive$Area)
ng2 <- l_navgraph(oliveAcids, separator='-', color=olive$Area)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_navgraph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_nestedTclList2Rlist")
### * l_nestedTclList2Rlist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_nestedTclList2Rlist
### Title: Convert a Nested Tcl List to an R List
### Aliases: l_nestedTclList2Rlist

### ** Examples


tclobj <- .Tcl('set a {{1 2 3} {2 3 4 4} {3 5 3 3}}')
l_nestedTclList2Rlist(tclobj)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_nestedTclList2Rlist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_ng_plots.default")
### * l_ng_plots.default

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_ng_plots.default
### Title: Select 2d spaces with variable associated measures displayed in
###   scatterplot matrix
### Aliases: l_ng_plots.default

### ** Examples

n <- 100
dat <- data.frame(
   A = rnorm(n), B = rnorm(n), C = rnorm(n),
   D = rnorm(n), E = rnorm(n)
)
m2d <- data.frame(
    cov = with(dat, c(cov(A,B), cov(A,C), cov(B,D), cov(D,E), cov(A,E))),
    measure_1 = c(1, 3, 2, 1, 4),
    row.names = c('A:B', 'A:C', 'B:D', 'D:E', 'A:E')
)

# or m2d <- as.matrix(m2d)

nav <- l_ng_plots(measures=m2d, data=dat)

# only one measure
m <- m2d[,1]
names(m) <- row.names(m2d)
nav <- l_ng_plots(measures=m, data=dat)

m2d[c(1,2),1]

# one d measures
m1d <- data.frame(
     mean = sapply(dat, mean),
     median =  sapply(dat, median),
     sd =  sapply(dat, sd),
     q1 = sapply(dat, function(x)quantile(x, probs=0.25)),
     q3 = sapply(dat, function(x)quantile(x, probs=0.75)),
     row.names = names(dat)
)

nav <- l_ng_plots(m1d, dat) 

## more involved
q1 <- function(x)as.vector(quantile(x, probs=0.25))

# be carful that the vector names are correct
nav <- l_ng_plots(sapply(oliveAcids, q1), oliveAcids)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_ng_plots.default", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_ng_plots.measures")
### * l_ng_plots.measures

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_ng_plots.measures
### Title: 2d Navigation Graph Setup with dynamic node fitering using a
###   scatterplot matrix
### Aliases: l_ng_plots.measures

### ** Examples

## Not run: 
##D # 2d measures
##D scags <- scagnostics2d(oliveAcids, separator='**')
##D scags()
##D ng <- l_ng_plots(scags, color=olive$Area)
##D 
##D # 1d measures
##D scale01 <- function(x){(x-min(x))/diff(range(x))}
##D m1d <- measures1d(sapply(iris[,-5], scale01),
##D      mean=mean, median=median, sd=sd,
##D      q1=function(x)as.vector(quantile(x, probs=0.25)),
##D      q3=function(x)as.vector(quantile(x, probs=0.75)))
##D 
##D m1d()
##D 
##D nav <- l_ng_plots(m1d, color=iris$Species)
##D 
##D # with only one measure
##D nav <- l_ng_plots(measures1d(oliveAcids, sd))
##D 
##D # with two measures
##D nav <- l_ng_plots(measures1d(oliveAcids, sd=sd, mean=mean))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_ng_plots.measures", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_ng_plots.scagnostics")
### * l_ng_plots.scagnostics

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_ng_plots.scagnostics
### Title: 2d Navigation Graph Setup with dynamic node fitering based on
###   scagnostic measures and by using a scatterplot matrix
### Aliases: l_ng_plots.scagnostics

### ** Examples


library(scagnostics)
scags <- scagnostics(oliveAcids)

l_ng_plots(scags, oliveAcids, color=olive$Area)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_ng_plots.scagnostics", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_ng_ranges.default")
### * l_ng_ranges.default

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_ng_ranges.default
### Title: Select 2d spaces with variable associated measures using a
###   slider
### Aliases: l_ng_ranges.default

### ** Examples

# Simple example with generated data
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

# or m2d <- as.matrix(m2d)

nav <- l_ng_ranges(measures=m2d, data=dat)

# With 1d measures
m1d <- data.frame(
     mean = sapply(dat, mean),
     median =  sapply(dat, median),
     sd =  sapply(dat, sd),
     q1 = sapply(dat, function(x)quantile(x, probs=0.25)),
     q3 = sapply(dat, function(x)quantile(x, probs=0.75)),
     row.names = names(dat)
)

nav <- l_ng_ranges(m1d, dat) 





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_ng_ranges.default", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_ng_ranges.measures")
### * l_ng_ranges.measures

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_ng_ranges.measures
### Title: 2d Navigation Graph Setup with dynamic node fitering using a
###   slider
### Aliases: l_ng_ranges.measures

### ** Examples

# 2d measures
# s <- scagnostics2d(oliveAcids)
# nav <- l_ng_ranges(s, color=olive$Area)

# 1d measures
scale01 <- function(x){(x-min(x))/diff(range(x))}
m1d <- measures1d(sapply(iris[,-5], scale01),
     mean=mean, median=median, sd=sd,
     q1=function(x)as.vector(quantile(x, probs=0.25)),
     q3=function(x)as.vector(quantile(x, probs=0.75)))

m1d()

nav <- l_ng_ranges(m1d, color=iris$Species)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_ng_ranges.measures", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_ng_ranges.scagnostics")
### * l_ng_ranges.scagnostics

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_ng_ranges.scagnostics
### Title: 2d Navigation Graph Setup with dynamic node fitering based on
###   scagnostic measures and using a slider
### Aliases: l_ng_ranges.scagnostics

### ** Examples

library(scagnostics)
s <- scagnostics(oliveAcids)
ng <- l_ng_ranges(s, oliveAcids, color=olive$Area)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_ng_ranges.scagnostics", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_pairs")
### * l_pairs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_pairs
### Title: Scatterplot Matrix in Loon
### Aliases: l_pairs

### ** Examples

p <- l_pairs(iris[,-5], color=iris$Species)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_pairs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_plot")
### * l_plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_plot
### Title: Create an interactive loon plot widget
### Aliases: l_plot

### ** Examples

# ordinary use
p <- with(iris, l_plot(Sepal.Width, Petal.Length, color=Species))

# link another plot with the previous plot
p['linkingGroup'] <- "iris_data"
p2 <- with(iris, l_plot(Sepal.Length, Petal.Width, linkingGroup="iris_data"))

# Use with other tk widgets
library(tcltk)
tt <- tktoplevel()
p1 <- l_plot(parent=tt, x=c(1,2,3), y=c(3,2,1))
p2 <- l_plot(parent=tt, x=c(4,3,1), y=c(6,8,4))

tkgrid(p1, row=0, column=0, sticky="nesw")
tkgrid(p2, row=0, column=1, sticky="nesw")
tkgrid.columnconfigure(tt, 0, weight=1)
tkgrid.columnconfigure(tt, 1, weight=1)
tkgrid.rowconfigure(tt, 0, weight=1)

tktitle(tt) <- "Loon plots with custom layout"



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_plot.default")
### * l_plot.default

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_plot.default
### Title: Create an interactive 2d scatterplot display
### Aliases: l_plot.default

### ** Examples

p1 <- with(iris, l_plot(Sepal.Length, Sepal.Width, color=Species))

p2 <- with(iris, l_plot(Petal.Length ~ Petal.Width, color=Species))

# link the two plots p1 and p2
l_configure(p1, linkingGroup="iris", sync="push")
l_configure(p2, linkingGroup="iris", sync="push")
p1['selected'] <- iris$Species == "setosa" 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_plot.default", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_plot.map")
### * l_plot.map

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_plot.map
### Title: Create an plot with a map layered
### Aliases: l_plot.map

### ** Examples

library(maps)
p <- l_plot(map('world', fill=TRUE, plot=FALSE))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_plot.map", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_plot_inspector")
### * l_plot_inspector

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_plot_inspector
### Title: Create a Scatterplot Inspector
### Aliases: l_plot_inspector

### ** Examples

i <- l_plot_inspector()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_plot_inspector", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_plot_inspector_analysis")
### * l_plot_inspector_analysis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_plot_inspector_analysis
### Title: Create a Scatterplot Analysis Inspector
### Aliases: l_plot_inspector_analysis

### ** Examples

i <- l_plot_inspector_analysis()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_plot_inspector_analysis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_redraw")
### * l_redraw

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_redraw
### Title: Force a Content Redraw of a Plot
### Aliases: l_redraw

### ** Examples

p <- l_plot(iris)
l_redraw(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_redraw", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_resize")
### * l_resize

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_resize
### Title: Resize Plot Widget
### Aliases: l_resize

### ** Examples

p <- l_plot(iris)

l_resize(p, 300, 300)
l_size(p) <- c(500, 500)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_resize", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_serialaxes")
### * l_serialaxes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_serialaxes
### Title: Create a Serialaxes Widget
### Aliases: l_serialaxes

### ** Examples

s <- l_serialaxes(data=oliveAcids, color=olive$Area, title="olive data")
s['axesLayout'] <- 'parallel'
states <- l_info_states(s)
names(states)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_serialaxes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_serialaxes_inspector")
### * l_serialaxes_inspector

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_serialaxes_inspector
### Title: Create a Serialaxes Analysis Inspector
### Aliases: l_serialaxes_inspector

### ** Examples

i <- l_serialaxes_inspector()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_serialaxes_inspector", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_setAspect")
### * l_setAspect

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_setAspect
### Title: Set the aspect ratio of a plot
### Aliases: l_setAspect

### ** Examples

p <- with(iris, l_plot(Sepal.Length ~ Sepal.Width, color=Species))

l_aspect(p)
l_setAspect(p, x = 1, y = 2) 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_setAspect", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_setColorList")
### * l_setColorList

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_setColorList
### Title: Use custom colors for mapping nominal values to distinct colors
### Aliases: l_setColorList

### ** Examples


l_plot(1:3, color=1:3) # loon's default mapping

cols <- l_getColorList()
l_setColorList(c("red", "blue", "green", "orange"))

## close and reopen inspector

l_plot(1:3, color=1:3)   # use the new color mapping
l_plot(1:10, color=1:10) # use loons default color mapping as color list is too small
 
# reset to default
l_setColorList(cols)

## Not run: 
##D # you can also perform the color mapping yourself, for example with
##D # the col_numeric function provided in the scales package
##D library(scales)
##D p_custom <- with(olive, l_plot(stearic ~ oleic,
##D      color = col_numeric("Greens", domain = NULL)(palmitic)))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_setColorList", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_setColorList_ColorBrewer")
### * l_setColorList_ColorBrewer

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_setColorList_ColorBrewer
### Title: Set loon's color mapping list to the colors from ColorBrewer
### Aliases: l_setColorList_ColorBrewer

### ** Examples


## Not run: 
##D library(RColorBrewer)
##D display.brewer.all()
## End(Not run)

l_setColorList_ColorBrewer("Set1")
p <- l_plot(iris)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_setColorList_ColorBrewer", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_size-set")
### * l_size-set

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_size<-
### Title: Resize Plot Widget
### Aliases: l_size<-

### ** Examples

p <- l_plot(iris)

l_resize(p, 300, 300)
l_size(p) <- c(500, 500)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_size-set", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l_worldview")
### * l_worldview

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l_worldview
### Title: Create a Worldview Inspector
### Aliases: l_worldview

### ** Examples

i <- l_worldview()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l_worldview", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("linegraph.loongraph")
### * linegraph.loongraph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: linegraph.loongraph
### Title: Create a linegraph of a graph
### Aliases: linegraph.loongraph

### ** Examples

g <- loongraph(letters[1:4], letters[1:3], letters[2:4], FALSE)

linegraph(g)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("linegraph.loongraph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("loon_palette")
### * loon_palette

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: loon_palette
### Title: Loon's color generator for creating color palettes
### Aliases: loon_palette

### ** Examples

loon_palette(12)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("loon_palette", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("loongraph")
### * loongraph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: loongraph
### Title: Create a graph object of class loongraph
### Aliases: loongraph

### ** Examples

g <- loongraph(
  nodes = c("A", "B", "C", "D"),
  from = c("A", "A", "B", "B", "C"),
  to   = c("B", "C", "C", "D", "D")
)

## Not run: 
##D # create a loon graph plot
##D p <- l_graph(g)
## End(Not run)

lg <- linegraph(g)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("loongraph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("make_glyphs")
### * make_glyphs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: make_glyphs
### Title: Make arbitrary glyphs with R graphic devices
### Aliases: make_glyphs

### ** Examples

## Not run: 
##D data(minority)
##D p <- l_plot(minority$long, minority$lat)
##D 
##D library(maps)
##D canada <- map("world",  "Canada", fill=TRUE, plot=FALSE)
##D l_map <- l_layer(p, canada, asSingleLayer=TRUE)
##D l_scaleto_world(p)
##D 
##D img <- make_glyphs(lapply(1:nrow(minority), function(i)minority[i,]), function(m) {
##D     par(mar=c(1,1,1,1)*.5)
##D     mat <- as.matrix(m[1,1:10]/max(m[1:10]))
##D     barplot(height = mat,
##D             beside = FALSE,
##D             ylim = c(0,1),
##D             axes= FALSE,
##D             axisnames=FALSE)
##D }, width=120, height=120)
##D 
##D l_imageviewer(img)
##D 
##D g <- l_glyph_add_image(p, img, "barplot")
##D p['glyph'] <- g
##D 
##D 
##D ## with grid
##D li <- make_glyphs(runif(6), function(x) {
##D     if(any(x>1 | x<0))
##D         stop("out of range")
##D     pushViewport(plotViewport(unit(c(1,1,1,1)*0, "points")))
##D     grid.rect(gp=gpar(fill=NA))
##D     grid.rect(0, 0, height = unit(x, "npc"), just = c("left", "bottom"),
##D               gp=gpar(col=NA, fill="steelblue"))
##D })
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("make_glyphs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("measures1d")
### * measures1d

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: measures1d
### Title: Closure of One Dimensional Measures
### Aliases: measures1d

### ** Examples

m1 <- measures1d(oliveAcids, mean=mean, median=median,
     sd=sd, q1=function(x)as.vector(quantile(x, probs=0.25)),
     q3=function(x)as.vector(quantile(x, probs=0.75)))
     
m1
m1()
m1(olive$palmitoleic>100)
m1('data')
m1('measures')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("measures1d", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("measures2d")
### * measures2d

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: measures2d
### Title: Closure of Two Dimensional Measures
### Aliases: measures2d

### ** Examples

m <- measures2d(oliveAcids, separator='*', cov=cov, cor=cor)
m
m()
m(keep=olive$palmitic>1360)
m('data')
m('grid')
m('measures')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("measures2d", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ndtransitiongraph")
### * ndtransitiongraph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ndtransitiongraph
### Title: Create a n-d transition graph
### Aliases: ndtransitiongraph

### ** Examples

g <- ndtransitiongraph(nodes=c('A:B', 'A:F', 'B:C', 'B:F'), n=3, separator=':')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ndtransitiongraph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.loongraph")
### * plot.loongraph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.loongraph
### Title: Plot a loon graph object with base R graphics
### Aliases: plot.loongraph

### ** Examples

library(Rgraphviz)
g <- loongraph(letters[1:4], letters[1:3], letters[2:4], FALSE)
plot(g)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.loongraph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("scagnostics2d")
### * scagnostics2d

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: scagnostics2d
### Title: Closure of Two Dimensional Scagnostic Measures
### Aliases: scagnostics2d

### ** Examples

m <- scagnostics2d(oliveAcids, separator='**')
m
m()
m(olive$palmitoleic > 80)
m('data')
m('grid')
m('measures')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("scagnostics2d", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tkcolors")
### * tkcolors

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tkcolors
### Title: List the valid Tk color names
### Aliases: tkcolors

### ** Examples

# check if R colors names and TK color names are the same
setdiff(tolower(colors()), tolower(tkcolors()))
setdiff(tolower(tkcolors()), tolower(colors()))
 
# hence there are currently more valid color names in Tk than there are in R

# Lets compare the colors of the R color names in R and Tk
tohex <- function(x) {
    sapply(x, function(xi) {
        crgb <- as.vector(col2rgb(xi))
        rgb(crgb[1], crgb[2], crgb[3], maxColorValue = 255)    
    })
}

df <- data.frame(
    R_col = tohex(colors()),
    Tcl_col = loon:::hex12tohex6(l_hexcolor(colors())),
    row.names = colors(),
    stringsAsFactors = FALSE
)

df_diff <- df[df$R_col != df$Tcl_col,]

library(grid)
grid.newpage()
pushViewport(plotViewport())

x_col <- unit(0, "npc")
x_R <- unit(6, "lines")
x_Tcl <- unit(10, "lines")

grid.text('color', x=x_col, y=unit(1, "npc"), just='left', gp=gpar(fontface='bold'))
grid.text('R', x=x_R, y=unit(1, "npc"), just='center', gp=gpar(fontface='bold'))
grid.text('Tcl', x=x_Tcl, y=unit(1, "npc"), just='center', gp=gpar(fontface='bold'))
for (i in 1:nrow(df_diff)) {
    y <- unit(1, "npc") - unit(i*1.2, "lines")
    grid.text(rownames(df_diff)[i], x=x_col, y=y, just='left')
    grid.rect(x=x_R, y=y, width=unit(3, "line"),
              height=unit(1, "line"), gp=gpar(fill=df_diff[i,1]))
    grid.rect(x=x_Tcl, y=y, width=unit(3, "line"),
              height=unit(1, "line"), gp=gpar(fill=df_diff[i,2]))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tkcolors", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
