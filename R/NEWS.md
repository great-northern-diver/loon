# loon 1.2.3

* added l_saveStatesRDS()
    - saves named or all states to disk for later retrieval (e.g. in Rmarkdown)
* added l_copyStates(source, target)
    - copies the info_state values of source to that of target.
    - some basic states are excluded but can be overridden.

# loon 1.2.2

* added l_plot3D contributed by Martin Gauch (for rwo)
* l_pairs linking issues and arguments for histograms and serialaxes in the display are fixed
* added a new vignette "loon and RMarkdown" and tidied up some others
* new functionality to make it easy to construct statistical glyphs from any R plotting function
    - new demo(l_make_glyphs) to illustrate statistical glyphs
    - tiny new data set oliveLocations for lat and long of olive data growing areas (used in demo)
    - tidied up a couple of demos (very superficially)

# loon 1.2.1

* updated `loonGrob()` and `grid.loon()`
    - provide more meaningful grobs (including better descriptions and more informative "empty" grobs).  
    - added `condGrob()` to effect the conditional construction of either the desired grob (if it is visible in the loon plot) or the empty grob as placeholder containing relevant arguments
 * introduced the method `plot.loon()` so that `plot(p)` of a loon plot `p` will now draw the corresponding grid object on the current device
    
# loon 1.2.0

* added `loonGrob()` and `grid.loon()` functionality 
    - any loon plot can now be converted and saved as a grid graphics object
    - permits printing/exporting snapshots of loon plots in documentation
    - S3 class structure extended to more loon plots
    
* `l_compound` classes introduced to ease user creation of complex layouts
    - includes simple programming interface to extend `loonGrob()` to any user-defined 
    - converted current compound layouts to new `l_compound` model (e.g. `l_pairs`, `l_stl`)
    - `l_getPlots` which returns a list of the loon plots in the display (works for simple loon plots too)
    - `l_getLocations` which returns a matrix showing the location of the loon plots (in order)   (works for simple loon plots too)
    - `l_get_arrangeGrobArgs` which returns the arguments and their values to be passed on to arrangeGrob() (from gridExtra) to allow arbitrary layout by the user.
      - this is NOT implemented for simple loon plots (but could perhaps be)
    - `loonGrob_layoutType` which returns a string indicating whether `"location"` or `"arrangeGrobArgs`" are to be used when creating a grid version of the `l_compound`.  (default is `"locations"`,  other possibility is `"arrangeGrobArgs"`


* Fixed widget handle class order: specific classes first.

* `l_hexcolor.R`

* `l_plot_decomposed_ts`

* `l_cget` and `l_configure` are now a generics with methods for objects of class `loon`, `character`, and `l_compound`.

* New functions `l_getOption`, `l_getOptionNames`, `l_userOptions`, `l_userOptionDefault`, `l_setOption`

* New function `l_primitiveGlyphs`

* `names` for loon object handles to print out the state names. `l_state_names` can be used alternatively





        
# loon 1.1.0
* first release on CRAN
* change state names 
    - `itemlabel` to `itemLabel`
    - `showItemlabels` to `showItemLabels`
* automatically convert nested lists and data.frames
    - unexported `l_nestedTclList2Rlist` and `l_data`
* remove the local copy of the website and make `l_help` point to the official website.

# loon 1.0.1
* Bug fix in geodesic2d for 4d transitions

# loon 0.9.1
* Bug fix when layering maps (sp classes) with asSingleLayer=TRUE
* Bug fix in scale_image

# loon 0.9
* initial beta release for the general public
