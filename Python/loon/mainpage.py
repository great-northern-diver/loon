'''
\mainpage loon manual

Some general info.

This manual is divided in the following sections:
- \subpage intro
- \subpage Reference 

-----------------------------------------------------------

\page intro [Introduction](@ref MyOtherPageName)


-----------------------------------------------------------

\page Reference Reference
  # Package Information
    desc:  General Information about the package
    contents:
    - loon
    - l_help
    - l_web
  # Main Plotting Functions
    desc:  These are the main functions needed to create plots
    contents:
    - l_plot_arguments
    - l_plot
    - l_hist
    - l_plot3D
    - l_pairs
    - l_serialaxes
    - plot.loon
    - plot.loongraph
  # Access Modify
    desc: Access Modify
    contents:
    - l_cget
    - l_configure
    - l_info_states
    - l_state_names
    - names.loon
    - l_getLinkedStates
    - l_setLinkedStates
    - l_getOption
    - l_setOption
    - l_userOptions
    - l_getOptionNames
    - l_getPlots
    - starts_with("l_scale")
    - starts_with("l_move")
    - l_redraw
    - l_size
    - l_size<-
    - l_zoom
    - l_copyStates
    - l_saveStatesRDS
  # Miscellaneous
    desc: Miscellaneous
    contents:
    - l_resize
    - l_export
    - l_export_valid_formats
    - l_aspect<-
    - l_aspect
    - l_setAspect
    - l_subwin
    - l_widget
    - l_setTitleFont
    - l_getLocations
    - l_userOptionDefault

  # Colors
    desc: Colors
    contents:
    - color_loon
    - loon::tkcolors
    - hex12tohex6
    - l_hexcolor
    - l_getColorList
    - loon_palette
    - l_colRemoveAlpha
    - starts_with("l_setColor")
  # Layering
    desc: Layers
    contents:
    - starts_with("l_layer")
    - print.l_layer
  # Glyphs
    desc: Glyphs
    contents:
    - starts_with("l_glyph")
    - l_primitiveGlyphs
    - starts_with("l_image")
    - l_make_glyphs
  # Bindings
    desc: Bindings
    contents:
    - starts_with("l_bind")
    - starts_with("l_current")
    - l_after_idle
  # Graph
    desc: Graph
    contents:
    - starts_with("l_graph")
    - as.graph
    - as.loongraph
    - complement
    - complement.loongraph
    - completegraph
    - graphreduce
    - loongraph
    - linegraph
    - linegraph.loongraph
    - l_getGraph
    - ndtransitiongraph
  # Navigation Graphs
    desc: Navigation
    contents:
    - starts_with("l_ng")
    - starts_with("l_nav")
    - l_create_handle
  # Contexts
    desc: Contexts are
    contents:
    - starts_with("l_context")
  # Grid Grobs
    desc: Grid Grobs
    contents:
    - loonGrob
    - grid.loon
    - condGrob
    - l_createCompoundGrob
    - l_get_arrangeGrobArgs
    - loonGrob_layoutType
  # Data
    desc: data
    contents:
    - UsAndThem
    - olive
    - oliveAcids
    - oliveLocations
    - minority
  # Inspectors
    desc: inspectors
    contents:
    - ends_with("inspector")
    - l_hist_inspector_analysis
    - l_worldview
  # Others
    desc: others
    contents:
    - l_list2nestedTclList
    - l_nestedTclList2list
    - l_data
    - l_throwErrorIfNotLoonWidget
    - l_toR
    - l_isLoonWidget
    - L2_distance
  # Measures
    desc: measures
    contents:
    - measures1d
    - measures2d
    - scagnostics2d
    - print.measures1d
    - print.measures2d

'''