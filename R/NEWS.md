# loon 1.2.0.9000
* in development
* added `loonGrob()` and `grid.loon()` functionality 
    - any loon plot can now be saved as a grid graphics object
    - permits printing/exporting snapshots of loon plots in documentation
    - S3 class structure extended to more loon plots
    - `l_compound` classes introduced to ease user creation of complex layouts
        - includes simple programming interface to extend `loonGrob()` to any user-defined `l_compound`
    - converted current compound layouts to new `l_compound` model (e.g. `l_pairs`, `l_stl`)
        
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
