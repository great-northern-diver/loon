## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
    echo = TRUE, 
    tidy.opts = list(width.cutoff = 65),
    tidy = TRUE)

set.seed(12314159)

imageDirectory <- "./images/loonRmarkdown"
dataDirectory <- "data"
path_concat <- function(path1, path2, sep="/") {paste(path1, path2, sep = sep)}

## ----loon, message = FALSE-----------------------------------------------
library(loon)

## ----iris l_plot, message = FALSE----------------------------------------
p <- l_plot(x = iris$Sepal.Width, y = iris$Sepal.Length, 
            color = iris$Species, 
            xlabel = "sepal width", ylabel = "sepal length",
            title = "The famous Iris data", 
            showGuides = TRUE)

## ----plot l_plot, out.width = "60%", fig.align = "center", fig.width = 6, fig.height = 5----
gp <- plot(p)
gp

## ----plot no draw l_plot, out.width = "60%", fig.align = "center", fig.width = 6, fig.height = 5----
gp <- plot(p, draw = FALSE)
gp

## ----gp png loon, eval = FALSE, message = FALSE, warning = FALSE, error = FALSE----
#  png(filename = "images/loonplot1.png", width = 600, height = 500)
#  grid::grid.draw(gp)
#  dev.off()

## ----knitr png, eval = FALSE, out.width = "60%", fig.align = "center", fig.width = 6, fig.height = 5----
#  knitr::include_graphics("images/loonplot1.png")

## ----knitr png real, echo = FALSE, out.width = "60%", fig.align = "center", fig.width = 6, fig.height = 5----
knitr::include_graphics(path_concat(imageDirectory, "loonplot1.png"))

## ----l_saveStatesRDS, eval = TRUE, message = FALSE, warning = FALSE, error = FALSE----
# Having determined the colours you could save them (and other states)
# in a file of your choice, here some tempfile:
myFileName <- tempfile("myPlot", fileext = ".rds")

#
# We could save all of the "usual" states 
# (excludes certain "basic" states, see help(l_saveStatesRDS))
l_saveStatesRDS(p, file = myFileName)

# 
# Or simply save selected named states as an RDS
l_saveStatesRDS(p,
                states = c("color", "active", "selected"),
                file = myFileName)

## ----readRDS, eval = TRUE, message = FALSE, warning = FALSE, error = FALSE----
# We have a new plot (or two)
p_new <- l_plot(iris, showGuides = TRUE)
h_new <- l_hist(iris$Sepal.Width, 
                showBinHandle = FALSE, 
                yshows = "density", 
                showStackedColors = TRUE)

# And read the saved data back in using R's readRDS() function
p_saved_info <- readRDS(myFileName)
# which is an object of class 
class(p_saved_info)
# The values on p_saved_info can now be all set using l_copyStates()
l_copyStates(source = p_saved_info, target = p_new)
# or selectively and even from different classes of loon plots
h_new["color"] <- p_saved_info$color

## ----plot the saved states, eval = TRUE, message = FALSE, warning = FALSE, error = FALSE, fig.align = "center", fig.width = 6, fig.height = 5, out.width = "50%"----
plot(p_new)

## ----plot the saved histogram, eval = TRUE, message = FALSE, warning = FALSE, error = FALSE, fig.align = "center", fig.width = 6, fig.height = 5, out.width = "50%"----
plot(h_new)

## ----multiple loon plots, eval = TRUE, out.width = "60%", fig.align = "center", fig.width = 6, fig.height = 5----
saveTitle <- p["title"]

p['title'] <- "1. Setosa selected"
p['selected'] <- iris$Species == "setosa"
gp_select <- plot(p, draw = FALSE)

p['title'] <- "2. Scale to the selected points"
l_scaleto_selected(p)
gp_select_zoom <- plot(p, draw = FALSE)

p['title'] <- "3. Turn off selection"
p['selected'] <- FALSE
gp_setosa_zoom <- plot(p, draw = FALSE)

# Put the plot back to how it was originally
p["title"] <- saveTitle 
l_scaleto_plot(p)

## ----gridExtra multiple loon plots, message = FALSE, out.width = "100%", fig.align = "center", fig.width = 12, fig.height = 10----
library(gridExtra)
# which can now be arranged in sequence
grid.arrange(gp, gp_select, gp_select_zoom, gp_setosa_zoom)

## ----save many pngs, eval = FALSE, echo=TRUE, message = FALSE, warning = FALSE, error = FALSE----
#  png(filename = "images/loonplot2.png", width = 600, height = 500)
#  grid::grid.draw(gp_select)
#  dev.off()
#  png(filename = "images/loonplot3.png", width = 600, height = 500)
#  grid::grid.draw(gp_select_zoom)
#  dev.off()
#  png(filename = "images/loonplot4.png", width = 600, height = 500)
#  grid::grid.draw(gp_setosa_zoom)
#  dev.off()

## ----knitr several,  eval = FALSE, out.width = "60%", fig.align = "center", fig.width = 6, fig.height = 5, fig.show = "hold"----
#  knitr::include_graphics(path = "images/loonplot1.png")
#  knitr::include_graphics(path = "images/loonplot2.png")
#  knitr::include_graphics(path = "images/loonplot3.png")
#  knitr::include_graphics(path = "images/loonplot4.png")

## ----real knitr several,  echo = TRUE, out.width = "60%", fig.align = "center", fig.width = 6, fig.height = 5, fig.show = "hold"----
knitr::include_graphics(path = path_concat(imageDirectory, "loonplot1.png"))
knitr::include_graphics(path = path_concat(imageDirectory, "loonplot2.png"))
knitr::include_graphics(path = path_concat(imageDirectory, "loonplot3.png"))
knitr::include_graphics(path = path_concat(imageDirectory, "loonplot4.png"))

## ----grid raster funs and png package, eval = FALSE, message = FALSE, warning = FALSE, error = FALSE, out.width = "100%", fig.align = "center", fig.width = 12, fig.height = 10----
#  library(grid)
#  library(gridExtra)
#  library(png)
#  
#  img <- as.raster(readPNG(source = "images/loonplot1.png"))
#  gp <- rasterGrob(img)
#  
#  img <- as.raster(readPNG(source = "images/loonplot2.png"))
#  gp_select <- rasterGrob(img)
#  
#  img <- as.raster(readPNG(source = "images/loonplot3.png"))
#  gp_select_zoom <- rasterGrob(img)
#  
#  img <- as.raster(readPNG(source = "images/loonplot4.png"))
#  gp_setosa_zoom <- rasterGrob(img)
#  
#  grid.arrange(gp, gp_select, gp_select_zoom, gp_setosa_zoom)

## ----real grid raster funs and png package, echo = FALSE, message = FALSE, warning = FALSE, error = FALSE, out.width = "100%", fig.align = "center", fig.width = 12, fig.height = 10----
library(grid)
library(gridExtra)
library(png)

img <- as.raster(readPNG(source = path_concat(imageDirectory, "loonplot1.png")))
gp <- rasterGrob(img)

img <- as.raster(readPNG(source = path_concat(imageDirectory, "loonplot2.png")))
gp_select <- rasterGrob(img)

img <- as.raster(readPNG(source = path_concat(imageDirectory, "loonplot3.png")))
gp_select_zoom <- rasterGrob(img)

img <- as.raster(readPNG(source = path_concat(imageDirectory, "loonplot4.png")))
gp_setosa_zoom <- rasterGrob(img)

grid.arrange(gp, gp_select, gp_select_zoom, gp_setosa_zoom)

