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
