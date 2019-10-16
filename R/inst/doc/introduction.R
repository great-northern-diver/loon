## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
    echo = TRUE, 
    tidy.opts = list(width.cutoff = 65),
    tidy = TRUE)

set.seed(12314159)

imageDirectory <- "./images/intro"
dataDirectory <- "data"
path_concat <- function(path1, path2, sep="/") {paste(path1, path2, sep = sep)}

## ----library_loon, eval = TRUE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 4, out.width = "75%", warning=FALSE, message=FALSE----
library(loon)

## ----first loon plot, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  l_plot(x = quakes$long,
#         y = quakes$lat,
#         xlabel = "longitude",
#         ylabel = "latitude",
#         title = "Tonga trench earthquakes")
#  
#  ## [1] ".l0.plot"
#  ## attr(,"class")
#  ## [1] "l_plot" "loon"

## ----loonQuakeOriginal, out.width= "60%", fig.align="center", echo=FALSE----
knitr::include_graphics(path_concat(imageDirectory, "quakesOriginal.png"))

## ----looninspectorQuakeOriginal, out.width= "30%", fig.align="center", echo=FALSE----
knitr::include_graphics(path_concat(imageDirectory, "inspectorQuakesOriginal.png"))

## ----panning, out.width= "40%", fig.align="center", echo=FALSE-----------
knitr::include_graphics(path_concat(imageDirectory, "panning.png"))

## ----zooming, out.width= "40%", fig.align="center", echo=FALSE-----------
knitr::include_graphics(path_concat(imageDirectory, "zooming.png"))

## ----point selection, out.width= "40%", fig.align="center", echo=FALSE----
knitr::include_graphics(path_concat(imageDirectory, "selecting.png"))

## ----select defaults, out.width= "40%", fig.align="left", echo=FALSE-----
knitr::include_graphics(path_concat(imageDirectory, "inspectorSelectDefaults.png"))

## ----looninspector plus 10 cols, out.width= "30%", fig.align="center", echo=FALSE----
knitr::include_graphics(path_concat(imageDirectory, "inspectorQuakesPlus10Cols.png"))

## ----l_getColorList, eval = TRUE, tidy = TRUE----------------------------
l_getColorList()

## ----grDevices palette, eval = TRUE, tidy =TRUE--------------------------
palette()

## ----changing the inspector palette, eval = FALSE------------------------
#  l_setColorList_baseR()                             # base R palette
#  l_setColorList_ColorBrewer("Set2")                 # colorblind friendly choice from ColorBrewer
#  l_setColorList_hcl(luminance = 80)                 # set of hcl colours
#  l_setColorList_ggplot2()                           # ggplot2's palette
#  l_setColorList_loon()                              # default loon palette
#  l_setColorList(l_colRemoveAlpha(rainbow(5)))       # any set of colours without alpha

## ----assign loon plot, eval = TRUE, echo = TRUE, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy = FALSE----
# loon graphics (note that the result is assigned to p)
p <- l_plot(x = quakes$long, y = quakes$lat, 
            xlabel = "longitude", ylabel = "latitude",
            title = "Tonga trench earthquakes")

## ----create handle for loon plot, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy = FALSE----
#  # accessing the plot from its string representation
#  p <- l_create_handle(".l0.plot")

## ----printed representation loon plot, eval = TRUE, echo = TRUE, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy = FALSE----
p 

## ----plot of loon plot, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 5, fig.height = 5, out.width = "50%", warning=FALSE, message=FALSE, tidy = FALSE----
#  plot(p)

## ----names, eval = TRUE, echo = TRUE, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy = TRUE----
names(p)

## ----showGuides, eval = TRUE, echo = TRUE, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy = TRUE----
p["showGuides"]

## ----set up group cols, eval = TRUE, echo = FALSE------------------------
cols_saved <- c("red", "red", "orange", "red", "red", "orange", 
"black", "orange", "orange", "red", "red", "black", 
"red", "red", "black", "orange", "black", "red", 
"red", "red", "red", "black", "red", "orange", 
"red", "red", "black", "red", "red", "red", 
"red", "black", "orange", "red", "orange", "red", 
"black", "red", "red", "black", "orange", "orange", 
"red", "orange", "black", "orange", "orange", "black", 
"red", "orange", "red", "orange", "black", "red", 
"red", "red", "red", "red", "red", "red", 
"red", "red", "steelblue", "black", "red", "orange", 
"red", "red", "red", "orange", "orange", "orange", 
"black", "red", "red", "red", "orange", "black", 
"red", "orange", "orange", "red", "red", "orange", 
"red", "orange", "black", "red", "orange", "orange", 
"black", "black", "red", "black", "orange", "red", 
"orange", "red", "black", "red", "orange", "red", 
"red", "orange", "red", "orange", "orange", "black", 
"orange", "orange", "orange", "red", "red", "red", 
"red", "red", "steelblue", "steelblue", "black", "black", 
"black", "red", "red", "red", "orange", "black", 
"orange", "red", "orange", "orange", "orange", "red", 
"black", "red", "red", "black", "orange", "orange", 
"orange", "orange", "steelblue", "red", "black", "orange", 
"steelblue", "orange", "orange", "black", "red", "red", 
"orange", "black", "orange", "black", "black", "red", 
"black", "red", "black", "black", "red", "red", 
"black", "orange", "orange", "orange", "orange", "orange", 
"red", "black", "red", "red", "red", "red", 
"red", "orange", "red", "orange", "red", "red", 
"red", "orange", "orange", "red", "red", "orange", 
"red", "red", "red", "orange", "orange", "black", 
"red", "red", "orange", "red", "orange", "orange", 
"red", "red", "red", "red", "red", "orange", 
"steelblue", "red", "red", "red", "orange", "red", 
"orange", "red", "orange", "orange", "red", "red", 
"orange", "red", "orange", "red", "orange", "black", 
"orange", "red", "red", "black", "orange", "orange", 
"orange", "black", "red", "red", "red", "orange", 
"red", "red", "orange", "red", "black", "red", 
"red", "orange", "black", "red", "orange", "red", 
"red", "orange", "red", "black", "black", "black", 
"red", "black", "orange", "red", "orange", "black", 
"red", "red", "orange", "red", "black", "red", 
"red", "orange", "black", "black", "red", "red", 
"red", "red", "red", "red", "red", "red", 
"red", "red", "red", "red", "orange", "red", 
"black", "orange", "orange", "red", "red", "orange", 
"red", "red", "red", "black", "red", "red", 
"orange", "orange", "red", "red", "orange", "black", 
"steelblue", "orange", "orange", "red", "steelblue", "red", 
"red", "red", "red", "orange", "steelblue", "steelblue", 
"red", "red", "orange", "red", "red", "black", 
"red", "black", "black", "orange", "red", "orange", 
"black", "orange", "red", "black", "orange", "black", 
"orange", "red", "orange", "black", "red", "orange", 
"orange", "orange", "orange", "orange", "orange", "orange", 
"orange", "orange", "orange", "orange", "orange", "orange", 
"orange", "orange", "orange", "black", "red", "orange", 
"orange", "red", "black", "red", "orange", "black", 
"red", "orange", "red", "orange", "black", "orange", 
"red", "red", "red", "orange", "red", "orange", 
"red", "red", "orange", "orange", "red", "orange", 
"orange", "orange", "black", "black", "orange", "black", 
"red", "orange", "red", "orange", "black", "orange", 
"orange", "orange", "red", "red", "red", "red", 
"red", "orange", "red", "steelblue", "orange", "black", 
"orange", "red", "red", "red", "red", "black", 
"red", "orange", "orange", "orange", "black", "red", 
"orange", "black", "black", "orange", "orange", "orange", 
"red", "orange", "orange", "red", "orange", "orange", 
"red", "red", "black", "red", "red", "red", 
"red", "red", "orange", "orange", "black", "red", 
"red", "orange", "black", "orange", "black", "red", 
"orange", "orange", "orange", "red", "red", "red", 
"orange", "red", "black", "orange", "orange", "black", 
"red", "orange", "red", "red", "orange", "red", 
"red", "red", "orange", "red", "black", "orange", 
"red", "orange", "orange", "orange", "orange", "black", 
"orange", "orange", "orange", "orange", "red", "orange", 
"red", "red", "orange", "orange", "red", "orange", 
"orange", "red", "red", "steelblue", "orange", "black", 
"red", "red", "orange", "black", "orange", "orange", 
"red", "orange", "orange", "orange", "orange", "black", 
"red", "red", "black", "black", "black", "red", 
"orange", "orange", "orange", "red", "red", "orange", 
"black", "orange", "orange", "red", "orange", "red", 
"orange", "black", "orange", "red", "black", "black", 
"red", "orange", "black", "black", "orange", "black", 
"orange", "black", "red", "black", "black", "red", 
"black", "black", "black", "black", "black", "black", 
"black", "red", "orange", "red", "red", "black", 
"black", "orange", "orange", "orange", "orange", "orange", 
"red", "black", "red", "orange", "orange", "orange", 
"red", "orange", "orange", "red", "orange", "orange", 
"black", "orange", "red", "orange", "red", "orange", 
"red", "red", "red", "orange", "black", "red", 
"black", "red", "red", "orange", "black", "red", 
"red", "red", "red", "red", "black", "black", 
"orange", "black", "black", "red", "orange", "orange", 
"orange", "orange", "red", "red", "black", "orange", 
"orange", "orange", "red", "orange", "orange", "black", 
"black", "red", "orange", "red", "orange", "black", 
"red", "black", "orange", "orange", "red", "red", 
"black", "red", "orange", "red", "black", "red", 
"red", "orange", "orange", "red", "orange", "red", 
"orange", "black", "orange", "red", "red", "black", 
"orange", "orange", "red", "orange", "orange", "orange", 
"orange", "red", "red", "red", "black", "red", 
"steelblue", "black", "red", "orange", "red", "red", 
"orange", "red", "red", "red", "red", "red", 
"red", "red", "red", "orange", "red", "black", 
"red", "red", "black", "orange", "orange", "red", 
"red", "red", "black", "orange", "orange", "red", 
"orange", "black", "red", "red", "red", "red", 
"red", "red", "red", "red", "red", "red", 
"red", "orange", "black", "red", "black", "black", 
"red", "red", "orange", "orange", "red", "orange", 
"orange", "red", "orange", "black", "orange", "black", 
"red", "steelblue", "red", "orange", "orange", "red", 
"black", "red", "red", "red", "black", "black", 
"red", "red", "red", "red", "red", "orange", 
"orange", "red", "black", "orange", "orange", "red", 
"red", "orange", "red", "orange", "orange", "orange", 
"red", "orange", "orange", "orange", "red", "red", 
"red", "orange", "red", "black", "red", "black", 
"orange", "orange", "black", "orange", "orange", "red", 
"orange", "orange", "black", "black", "orange", "orange", 
"black", "orange", "orange", "red", "orange", "red", 
"red", "red", "orange", "red", "black", "red", 
"black", "black", "orange", "orange", "red", "orange", 
"black", "red", "orange", "red", "red", "red", 
"red", "orange", "red", "orange", "black", "red", 
"orange", "red", "orange", "red", "orange", "steelblue", 
"red", "orange", "red", "red", "red", "red", 
"red", "orange", "black", "orange", "red", "red", 
"orange", "orange", "red", "red", "red", "orange", 
"red", "red", "black", "red", "black", "orange", 
"orange", "orange", "orange", "orange", "orange", "red", 
"red", "orange", "black", "red", "orange", "black", 
"red", "orange", "orange", "black", "red", "red", 
"red", "red", "red", "red", "orange", "black", 
"black", "orange", "red", "orange", "steelblue", "orange", 
"orange", "black", "red", "red", "red", "red", 
"black", "black", "red", "orange", "black", "black", 
"red", "black", "black", "red", "red", "red", 
"red", "red", "red", "red", "orange", "red", 
"black", "black", "orange", "red", "black", "black", 
"orange", "orange", "black", "red", "black", "orange", 
"red", "red", "orange", "red", "orange", "red", 
"orange", "orange", "orange", "red", "red", "red", 
"red", "black", "black", "orange", "red", "black", 
"orange", "red", "black", "black", "orange", "orange", 
"orange", "red", "black", "orange", "red", "red", 
"orange", "red", "black", "orange", "orange", "black", 
"orange", "red", "red", "orange", "orange", "orange", 
"red", "orange", "red", "red", "red", "orange", 
"red", "orange", "orange", "orange", "red", "orange", 
"orange", "orange", "red", "orange", "orange", "red", 
"red", "red", "orange", "orange", "red", "orange", 
"orange", "black", "black", "orange", "orange", "red", 
"red", "red", "black", "orange", "orange", "orange", 
"orange", "black", "orange", "orange", "orange", "orange", 
"red", "black", "red", "black", "orange", "orange", 
"orange", "black", "black", "black", "red", "red", 
"red", "red", "orange", "red", "orange", "red", 
"black", "orange", "orange", "black"
)
p["color"] <- cols_saved


## ----group by colour, eval = TRUE, echo = TRUE, tidy = FALSE, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE----
unique_cols <- unique(p['color'])
group1 <- p['color'] == unique_cols[1]
# Number in group 1 (e.g. as you might have -- here from saved colours)
sum(group1)
# Data on first few quakes in group 1.
head(quakes[group1,])

## ----getGroups, eval = TRUE, echo = TRUE, tidy = FALSE, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE----
getGroups <- function(loonplot){
    # make sure it is an "l_plot"
    if (!is(loonplot, "l_plot")) stop("loonplot must be an l_plot")
    
    # use color to identify groups
    unique_cols <- unique(loonplot['color'])
    lapply(unique_cols,
           FUN = function(col){loonplot['color'] == col}
           )
}

myGroups <- getGroups(p)    # returns groups identified by unique colour in p
nGroups <- length(myGroups) # number of groups
group1 <- myGroups[[1]]     # each group is an element of the list myGroups

## ----set examples, eval = FALSE, echo = TRUE, tidy = FALSE, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE----
#  p["showGuides"] <- TRUE
#  
#  p["size"] <- sample(1:30, size = length(p["x"]), replace = TRUE)
#  
#  for (i in 1:length(myGroups))  {
#      p["selected"] <- myGroups[[i]]
#      Sys.sleep(1)
#  }
#  p["selected"] <- FALSE
#  
#  # something a little more involved for up to 6 groups
#  myCols <- c("firebrick", "steelblue", "purple",
#              "orange", "grey10", "grey80")
#  for (i in 1:length(myGroups)) {
#      p["color"][myGroups[[i]]] <- myCols[i]
#  }
#  
#  # something crazy
#  for (j in 1:10) {
#    p["xTemp"] <- p["x"] + runif(length(p["x"]), min = -0.5, max = 0.5)
#    Sys.sleep(0.1)
#  }
#  
#  # putting locations and size back
#  p["xTemp"] <- p["x"]
#  p["size"] <- 4
#  

## ----gettingAMap---------------------------------------------------------
library(maps)
NZFijiMap <- map("world2", 
                 regions=c("New Zealand", "Fiji"),
                 plot=FALSE)

## ----addingAMap,  eval = TRUE, echo = TRUE, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy = FALSE----
l_layer(p, NZFijiMap, 
        label = "New Zealand and Fiji",
        color = "forestgreen",
        index = "end")

## ----quakesGuidesMapInspector, out.width= "30%", fig.align="center", echo=FALSE----
knitr::include_graphics(path_concat(imageDirectory, "quakesGuidesMapInspector.png"))

## ----quakesGuidesMap, out.width= "50%", fig.align="center", echo=FALSE----
knitr::include_graphics(path_concat(imageDirectory, "quakesGuidesMap.png"))

## ----worldViewScaling, out.width= "90%", fig.align="center", echo=FALSE----
knitr::include_graphics(path_concat(imageDirectory, "quakesWorldViewScaling.png"))

## ----quakesLayersInspector, out.width= "70%", fig.align="center", echo=FALSE----
knitr::include_graphics(path_concat(imageDirectory, "quakesLayersInspector.png"))

## ----loon and base syntax, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  # Base graphics
#  plot(x = quakes$long, y = quakes$lat,
#       xlab = "longitude", ylab = "latitude",
#       main = "Tonga trench earthquakes")
#  
#  # loon graphics
#  l_plot(x = quakes$long, y = quakes$lat,
#         xlabel = "longitude", ylabel = "latitude",
#         title = "Tonga trench earthquakes")

## ----histogram of magnitude,  eval = TRUE, echo = TRUE, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy = FALSE----
h <- l_hist(quakes$depth, 
            xlabel = "depth", 
            title = "Tonga trench earthquakes")

## ----set linkingGroup mistake, eval = FALSE, warning=FALSE, message=FALSE, tidy = FALSE----
#  h["linkingGroup"] <- "quakes"
#  p["linkingGroup"] <- "quakes"

## ----fix colors again, eval = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "60%", warning=FALSE, message=FALSE, tidy = FALSE----
l_configure(h, linkingGroup = "quakes", sync = "push")
l_configure(p, linkingGroup = "quakes", sync = "pull")

## ----histogram blues, fig.align="center", fig.width = 7, fig.height = 5, out.width = "60%", warning=FALSE, message=FALSE, tidy = FALSE----
blues5 <- blues9[c(2,4,6,8,9)] # select 5 from light blue to dark
h["color"] <- blues5[cut(quakes$depth, breaks=5)] # assign colours by depth
p["glyph"] <- "ccircle"
p["showGuides"] <- TRUE
# plot(p) # will print the plot

## ----final quakes histograms, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy = FALSE----
h_mag <- l_hist(x = quakes$mag, 
                linkingGroup = "quakes", 
                showStackedColors = TRUE,
                yshows = "density",
                xlabel = "magnitude")

h_stations <- l_hist(x = quakes$stations, 
                     linkingGroup = "quakes", 
                     showStackedColors = TRUE,
                     yshows = "density",
                     xlabel = "Number of stations reporting")

## ----final quakes plot, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy = FALSE----
p_mag_stations <- l_plot(x = quakes$mag, 
                         y = quakes$stations, 
                         showGuides = TRUE,
                         glyph = "ccircle",
                         linkingGroup = "quakes", 
                         xlabel = "Magnitude", 
                         ylabel = "Number of stations reporting")

## ---- eval = FALSE-------------------------------------------------------
#  new_order <- sample(1:nrow(quakes), nrow(quakes), replace = FALSE)
#  some_quakes <-  sample(1:nrow(quakes), 100, replace = FALSE)
#  quakes_linkingKey <- p["linkingKey"]
#  p_new <- with(quakes[new_order,],
#                l_plot(long, lat, title = "quakes reordered",
#                       linkingGroup = "quakes",
#                       linkingKey = quakes_linkingKey[new_order] ))
#  p_subset <- with(quakes[some_quakes,],
#                l_plot(long, lat, title = "quakes reordered",
#                       linkingGroup = "quakes",
#                       linkingKey = quakes_linkingKey[some_quakes]))

## ----plot_3D,  fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy = FALSE----
# First, scale the data
scaled_quakes <- l_scale3D(quakes)

p_mag_stations <- l_plot3D(x = scaled_quakes$long,  
                           y = scaled_quakes$lat, 
                           z = scaled_quakes$depth, 
                           showGuides = TRUE,
                           linkingGroup = "quakes", 
                           title = "Three dimensional plot of quakes")

## ---- eval=FALSE---------------------------------------------------------
#  help(package = "loon")

## ---- eval=FALSE---------------------------------------------------------
#  l_help()
#  # or more simply via the online manual
#  l_web()

## ---- eval=FALSE---------------------------------------------------------
#  vignette("minorities", package = "loon")

## ---- eval=FALSE---------------------------------------------------------
#  vignette("teaching-example-smoothing", package = "loon")

## ---- eval=FALSE---------------------------------------------------------
#  vignette("Rmarkdown", package = "loon")

## ----loon demos, eval = FALSE--------------------------------------------
#  demo(package = "loon")  # list all demos

## ----loon demo examples, eval = FALSE------------------------------------
#  ### teaching demos
#  demo("l_regression",
#       package = "loon")  # lots using the Old Faithful geyser
#  demo("l_regression_influential",
#       package = "loon")  # move and recolor points to change the regression fit
#  
#  ### gapminder
#  demo("l_us_and_them",
#       package = "loon")  #  basic demo
#  demo("l_us_and_them_slider",
#       package = "loon")  # year selected on a slider
#  demo("l_us_and_them_choropleth",
#       package = "loon")  # world map and linked with a scatterplot
#  
#  ### the spatial package sp
#  demo("l_polygons_sp",
#       package = "loon")  # layer polygons with class sp
#  
#  ### layering and custom layouts
#  demo("l_layers")  # demonstrate layer types
#  demo("l_layout")  # custom layout widgets
#  demo("l_widgets") # inspector and plot in one window
#  
#  ### novel brushing and linking
#  demo("l_knn")     # brushing by k nearest points in some subspace
#  demo("l_us_and_them_choropleth")  # many to one linking
#  
#  ### high dimensional data and dimensionality reduction
#  demo("l_ng_images_frey_LLE") # navigation graphs, image data, LLE
#  demo("l_ng_dimred")          # comparing dimension reduction methods

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("zenplots")
#  vignette("intro", package = "zenplots")

## ----grid grob, eval = TRUE, echo = TRUE, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy = FALSE----
gp <- loonGrob(p)
gp

## ----list grid grob, eval = FALSE----------------------------------------
#  library(grid)
#  grid.ls(gp)  # lists the contents of the grob

## ----grid graphic display loon plot, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 5, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy = FALSE----
#  library(grid)
#  grid.newpage()
#  grid.loon(p)

