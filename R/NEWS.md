# loon 1.3.9

Some minor changes in facets:

  - in facet_wrap, as the number of facet variables increase, the layout pattern is not consistent (e.g., one variable: the layout is by row; more than one variables: the layout is by column).
  
  - fix typos: "extent" --> "extend"

# loon 1.3.8

* Removed the vignette "Example analysis" on the minority data to make room for a new vignette "Publishing loon plots"

* New helper functions for changing grobs produced by loonGrob()

  - `l_instantiateGrob`, `l_setGrobPlotView`, and `l_updateGrob`
  
* Export some useful helper functions:

    - `glyph_to_pch`: turn a loon point glyph to an R `graphics` plotting "character".
    
    - `get_display_color`: return the displayed color.
    
    - `as_hex6color`: return a 6 hexidecimal digit color representations.
    
    - `get_font_info_from_tk`: return font information.
    
    - `get_layer_states`: get layer states
    
    - `get_model_display_order`: get the order of the displayed elements, e.g., points in scatterplot and lines in serialaxes.
    
    - `tcl_img_2_r_raster`: turn a tk image object to an R raster object.
    
    - `char2num.data.frame`: a character data frame to a numerical data frame.

# loon 1.3.7

* A new function `l_colorName` is exported which returns the build-in color names based on the 12 or 6 hex code. 

* For `loonGrob`, remove the lines at the base of the tick marks

# loon 1.3.6

* fixed printing bug just introduced by change to facets. 

# loon 1.3.5

* `l_web()` now expanded to provide access to other `loon` related package documentation from the 
  `great-northern-diver` github organization's set of repositories (in anticipation of putting `diveR` on CRAN)
  
* Improved look of compound displays. (A lot of this is a result of changes to `loon.ggplot` package).
  
  Includes

  - new arguments `plotWidth` and `plotHeight` to `l_facet()` and `l_pairs()`
  - look of labels (identifying facetted rows and columns)
  - facetted displays now more loon consistent default argument look up; 
    i.e. now standardized via `l_getOption()`  (e.g. `bg = l_getOption("canvas_bg_guides")`)
  - more meaningful names given to `grobs` for facets. (as in `loonGrob()`)
    
* initial `tk` window sizes are now larger (`500 x 500` instead of `200 x 200`)

# loon 1.3.4

* A new vignette

  - "Logical queries in interactive graphics"
  
    Explains how to use loon's interactive plots and inspector to construct logical
    queries on the displayed data.
    
    An *enriched* version of `mtcars` is used for illustration.
    
    How a plot's `linkingKey` (from Loon's linking model) can be used to access the correct
    elements of any logical operation is also described.
    
* minor bug fixes and improvements

  - Three minor bug fixes on **facets**
  
    - If the `by` is a formula with `loon` n-dimensional states, no need to provide additional data, 
      the formula should be able to be converted to a data frame given by the states info.
      
    - When `by` has missing values, it is treated as other n-dimensional states: 
    
      drop NAs, leave warnings (which one are removed)
      
    - Removed unnecessary warnings.
  
  - `l_pairs()` has a progress bar for constructing, linking, and connecting the scales of the plots.
  
    there is now a new argument `showProgressBar` that allows this to be suppressed (e.g. in RMarkdown files)
    
  - for histograms of factors and character vectors:  default setting of arg `showFactors`
    
    - previously: `TRUE` whenever number of factor levels (or equivalent unique strings) < 10
    - now: `TRUE` whenever < 25 to accomodate common factors like `month` or the 24 hours in a day.
  
  - updated documentation on `l_layer()`
  
    
* testing
  
  - As all examples are wrapped in `if(interactive)`, check all interactive examples to ensure they work well
  
  - hexadecimal colours seem to get slightly (generally ignorable) different  hexadecimal values for  colours in `tcltk` on Solaris (see 1.3.3 comments).  this caused problems in testing for exact colours on Solaris.
  
    Until this is sorted out, the testing for colour hex values in the test suite were commented out to prevent loon from being unnecessarily archived on CRAN.  (The colours are correct, but our previous solution generated warnings, which may cause problems for CRAN acceptance.)  Likely, in the future, we suppress the warning from `l_hexcolor()` when testing.

   - for iris dataset in tests, we drop an observation so that for each species, the number of observations are different.

# loon 1.3.3

* an extremely minor update

  - test functions were changed to accommodate possibly inconsistent hex colours on Solaris OS.
    
    For example, in Solaris only, the hex12 for the color "red" appears both as "#FFFF00000000" and "#FF0000000000".  This can lead to failures of testing equality on the hex12 strings (though both appeared
    by creating the hex12 version of R's "red" in TCL, as in `l_hexcolor("red")`)
    
    Both hex 12 representations convert to hex 6 as "#FF0000".  That is, `hex12tohex6("#FFFF00000000")` and
    `hex12tohex6("#FF0000000000")` return `"#FF0000"` except that the second also generates a warning.

  - also took the opportunity to 
    
    - give more meaningful names to grobs when `loonGrob()` is called on an `l_compound`
    
    - update the documentation summary `l_plot_arguments` to include arguments `by`, `on`, `layout`, and `connectedScales` all of which are related to facetting plots at creation.
    
    - wrap the examples in  `Bin_cut` with `if (interactive()) {}`

# loon 1.3.2

* Updated linking to once again be more natural

  **Problem**: If a plot `p1` say already exists and participates in
  some linking group, say "mylinkingGroup", then creating a new plot in the same linking group
  via the command line console (that is not using the inspector) then some decisions on syncing the new plot's linked states need to be made.  
  
  To this end, the new plot can make decisions via the argument `sync` as in
  
  `p2 <- l_plot(x, linkingGroup = "myLinkingGroup", sync = "pull", ...)`
  
  or
  
  `p2 <- l_plot(x, linkingGroup = "myLinkingGroup", sync = "push", ...)`
  
  If a value for `sync` is not given, it will default to `sync = "pull"`, the idea being that the user intends to pull all linkable state values because they indicated having `p2` join the group at the time of its creation.
  
  What has to be considered is what happens if, in the remaining `...` arguments, the user simultaneously includes values for some of the linkable states such as `color = "red"`.  
  
  The solution below tries to infer some of the intention of the user, when it can, and warn when it cannot.
  
  
  **Solution**:  Suppose that `p1` exists as a plot in `linkingGroup = "myLinkingGroup"` and the user is now creating a new plot, say `p2`, with argument `linkingGroup = "myLinkingGroup"` given in the call to create `p2`.
  
  If  
  
  1. the argument `sync = "push"` is given in the call, then 
  
     a. if **any** linkable states are explicitly given as part of the call:
     
        - those **explicitly given** will have their values **pushed** to all members of the group; 
        - those **not explicitly given** explicitly will **not be pushed**.
    		
     b. if **no**  linkable states are explicitly given as part of the call:
     
        - **all** linkable states will have their **default values pushed** to the group **and** a warning given.
    		
	2. the argument is `sync = "pull"` (either by default or explicitly given to be `"pull"`) **and** the values are explicitly specified for some linkable states, then 
	   - these are ignored, and
	   - if they are different from the values of those states shared by the group, a warning is given.
	   

* For a `loon` histogram, if the input is a list, the histogram will be split into multiple panels. The `x` of each facet corresponds to an element in that list

*  A new feature has been added to `l_serialaxes(..., andrews = TRUE)` that will produce Andrews's Fourier functional curves for the data.

* A new statistical layer `l_layer_smooth` will generate a smooth for the active points and add it as a layer


*  Minor bugs were fixed/improvements made such as 

   - allowing a single point plot to be drawn with polygon glyph
   - having the pairs pan and zoom work when histograms are shown on the diagonal
   - progress bars added for pairs (and other l_compounds) when there are a lot of plots to be constructed.
   - compound plots (e.g. pairs plots) now accept any parent so that they can be later packed as part of some more complex user defined plot


# loon 1.3.1

* Added new functions l_loonWidgets() which returns widgets of all 
  displayed loon plots.  Related functions include
    - l_basePaths() and l_compoundPaths
    - l_getFromPath()
    
* Fixed bug so that l_make_glyphs() again works in Mac OS Catalina
  (Thanks to Brian Ripley and Simon Urbanek)
    - Cool new example there which produces the Periodic Table of the Elements
      as a scatterplot (data "elements" from loon.data package).  
      Actually see the periodicity by brushing.
  
* A minor bug fix with respect to facet label layout

* "color", "size", "linewidth", "glyph", added to 'l_userOptions()' 
    - default "color", "size", "linewidth", "glyph", etc. now set to
      look up default 'l_getOption("linewidth")' etc. 
    - this allows the user to change the default look of all plots
      simply by using 'l_setOption("color", "black")' for example.


# loon 1.3.0

* several new features added to loon plots
    - NAs are now accommodated
    - histograms recognize factors to produce barplots

* group by and facetting in loon plots
    - l_plot() and l_hist() now accept arguments by and facet
    
* improved docuentation
    - new vignettes
    - better organized reference manual via l_web()
    
* in some OSes the function l_image_import_files() has stopped working
    - seems to be a problem with the tcltk function tkimage.create()
    - tkimage.create() function works on older .png files but seemingly not 
    on newer ones
    - l_make_glyphs() calls l_image_import_files() and so although it will still create the necessary png files, it may fail to read them back in.
    - the problem has been observed (unreliably) to work if the png is further processed outside R.
    - so demo("l_make_glyphs") has been removed
    
    
# loon 1.2.3.9000

* deprecated l_saveStatesRDS() 
    - replaced by l_saveStates
    - reasoning for name change is to have the same name be used in 
    the Python release for the same functionality
    - introduced l_getSavedStates() to read the states (instead of readRDS for the
    same reason as above).
    
* updated l_cget()
    - Fixed reading of labels to retrieve raw characters so that braces etc. can appear in labels
    - updated l_hist.factor so that a layer of factor level labels appear below the corresponding bar
    
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
