## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE,
                      message = FALSE,
                      fig.align = "center", 
                      fig.width = 6, 
                      fig.height = 5,
                      out.width = "60%", 
                      tidy.opts = list(width.cutoff = 65),
                      tidy = FALSE)
set.seed(12314159)
library(loon.data)
library(loon)
library(gridExtra)

imageDirectory <- "./images/savingLoonPlots"
dataDirectory <- "./data/savingLoonPlots"
path_concat <- function(path1, ..., sep="/") {
    # The "/" is standard unix directory separator and so will
    # work on Macs and Linux.
    # In windows the separator might have to be sep = "\" or 
    # even sep = "\\" or possibly something else. 
    paste(path1, ..., sep = sep)
    }

## ----basic plots--------------------------------------------------------------
library(loon)
# First, make sure you always assign a loon plot to a variable
# at the same time that you create it.  This will give you access
# to it later.  
# (NB: If you haven't assigned it, you can later using l_getFromPath(). )
# 
#  A histogram, the name for the linkingGroup is arbitrary
h <- l_hist(iris$Sepal.Length, color = "grey", xlabel = "Sepal length",
            linkingGroup = "flowers")
#
# A scatterplot
p <- l_plot(iris$Petal.Width, iris$Petal.Length,  
            color = "grey", size = 10,
            xlabel = "Petal Width", ylabel = "Petal Length",
            linkingGroup = "flowers")
#
# A serial axes plot using the first 4 columns of iris (i.e. no Species info).
sa <- l_serialaxes(iris[, 1:4],   
            color = "grey", linkingGroup = "flowers")
#
# A static version of the plot can be easily produced.
plot(h)

## ----grid packages, fig.width = 16, fig.height = 5----------------------------
library(grid); library(gridExtra)

## ----several plots, fig.width = 16, fig.height = 5----------------------------
# First get the data stuctures corresponding to the static snapshots
# of the current plots.  Because we want to arrange them, we don't
# draw them at first.
plot_h <- plot(h, draw = FALSE)
plot_p <- plot(p, draw = FALSE)
plot_sa <- plot(sa, draw = FALSE)
# Now we draw them
grid.arrange(plot_h, plot_p, plot_sa, nrow = 1)

## ----several plots changed, fig.width = 16, fig.height = 5--------------------
# Change the colour by changing it *programmatically* on the plot
# 
h["color"] <- iris$Species  # use species to determine the colours
plot_h <- plot(h, draw = FALSE)
plot_p <- plot(p, draw = FALSE)
plot_sa <- plot(sa, draw = FALSE)
# And draw them again
grid.arrange(plot_h, plot_p, plot_sa, nrow = 1)

## ----plot states--------------------------------------------------------------
# Accessing values
head(sa["color"])  # will show the current colours of sa
# Note that these colours are expressed as 12 hexadecimal digits 
# This is what is used in TCL.  To turn them into R's 6 digit hexadecimal 
# (to use in other R static plots, for example) just use hex12tohex6()
hex12tohex6(sa["color"][1:3])
# Setting values ... make only specied iris versicolor active
sa["active"] <- iris$Species == "versicolor"
plot(sa)
# Setting values ... make everything active
sa["active"] <- TRUE
plot(sa)

## ----l_export, eval = FALSE---------------------------------------------------
#  l_export(p, filename = "myplot_via_l_export.pdf", height = 500, width = 600)

## ----png device, eval = FALSE-------------------------------------------------
#  # turn the png graphics device on with name of the file
#  png(file = "myplot_via_R.png", width = 600, height = 500)
#  # Draw the static plot
#  plot(p)
#  # Turn the device off/close the file
#  dev.off()

## ----show the saved plots, eval = FALSE---------------------------------------
#  # The one exported from RStudio
#  knitr::include_graphics("myplot_via_RStudio.png")
#  # Followed by the one saved using l_export
#  # (note that background grid is missing)
#  knitr::include_graphics("myplot_via_l_export.pdf")
#  # And finally, the one saved using R's png device
#  knitr::include_graphics("myplot_via_R.png")

## ----load the saved plots, echo = FALSE---------------------------------------
# The one exported from RStudio
knitr::include_graphics(path_concat(imageDirectory, "myplot_via_RStudio.png"))
# Followed by the one saved using l_export 
# (note that background grid is missing)
knitr::include_graphics(path_concat(imageDirectory, "myplot_via_l_export.pdf"))
# And finally, the one saved using R's png device
knitr::include_graphics(path_concat(imageDirectory, "myplot_via_R.png"))

## ----p saveStates, eval = FALSE-----------------------------------------------
#  l_saveStates(p, file = "p_savedStates")

## ----show get p states back, eval = FALSE-------------------------------------
#  p_savedStates <- l_getSavedStates(file = "p_savedStates")

## ----get p states back, echo = TRUE-------------------------------------------
p_savedStates <- l_getSavedStates(file = path_concat(dataDirectory, "p_savedStates"))

## ----new plot and copy--------------------------------------------------------
new_p <- l_plot(iris$Petal.Width, iris$Petal.Length)
l_copyStates(source = p_savedStates, target = new_p)

## ----plot new_p---------------------------------------------------------------
plot(new_p)

## ----default states-----------------------------------------------------------
names(p_savedStates)

## ----zoom and change glyphs on p, echo = FALSE--------------------------------
p["selected"] <- iris$Species == "versicolor"
l_move_jitter(p, which = "selected")
l_scaleto_selected(p)
p["selected"] <- FALSE
sample <- sample(1:nrow(iris), 50, replace = FALSE)
p["glyph"][sample] <- "ctriangle"
p["size"][sample] <- 30
plot(p)

## ----save states  zooming, eval = FALSE---------------------------------------
#  l_saveStates(p,
#               states = names(p),
#               file = "p_focusOnVersicolor")

## ---- eval = FALSE------------------------------------------------------------
#  p_focusOnVersicolor <- l_getSavedStates(file = "p_focusOnVersicolor")

## ---- echo = FALSE------------------------------------------------------------
p_focusOnVersicolor <- l_getSavedStates(file = path_concat(dataDirectory, "p_focusOnVersicolor"))

## ----copy all but basic states, eval = TRUE-----------------------------------
l_copyStates(source = p_focusOnVersicolor, 
             target = new_p, 
             returnNames = TRUE)

## ----a changed new_p----------------------------------------------------------
plot(new_p)

## ----copy all states, eval = TRUE---------------------------------------------
l_copyStates(source = p_focusOnVersicolor, 
             target = new_p, 
             excludeBasicStates = FALSE)

## ----final plot focus on versicolor-------------------------------------------
plot(new_p)

