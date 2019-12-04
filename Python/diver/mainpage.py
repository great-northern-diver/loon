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
    - loon.loon
    - loon.l_help
    - loon.l_web
  # Main Plotting Functions
    desc:  These are the main functions needed to create plots
    contents:
    - loon.l_plot
    - loon.l_hist
    - loon.l_plot3D
    - loon.l_pairs
    - loon.l_serialaxes
  # Access Modify
    desc: Access Modify
    contents:
    - loon.l_cget
    - loon.l_configure
    - loon.l_info_states
    - loon.l_state_names
    - loon.l_getLinkedStates
    - loon.l_setLinkedStates
    - loon.l_getOption
    - loon.l_setOption
    - loon.l_userOptions
    - loon.l_getOptionNames
    - loon.l_getPlots
    - loon.l_scale
    - loon.l_move
    - loon.l_redraw
    - loon.l_size
    - loon.l_zoom
    - loon.l_copyStates
    - loon.l_saveStates
  # Miscellaneous
    desc: Miscellaneous
    contents:
    - loon.l_resize
    - loon.l_export
    - loon.l_export_valid_formats
    - loon.l_aspect<-
    - loon.l_aspect
    - loon.l_setAspect
    - loon.l_subwin
    - loon.l_widget
    - loon.l_setTitleFont
    - loon.l_getLocations
    - loon.l_userOptionDefault

  # Colors
    desc: Colors
    contents:
    - loon.color_loon
    - loon.tkcolors
    - loon.hex12tohex6
    - loon.l_hexcolor
    - loon.l_getColorList
    - loon.loon_palette
    - loon.l_colRemoveAlpha
    - starts_with("l_setColor")
  # Layering
    desc: Layers
    contents:
    - loon.l_layer
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
    - loon.l_graph
    - loon.as.graph
    - loon.as.loongraph
    - loon.complement
    - loon.complement.loongraph
    - loon.completegraph
    - loon.graphreduce
    - loon.loongraph
    - loon.linegraph
    - loon.linegraph.loongraph
    - loon.l_getGraph
    - loon.ndtransitiongraph
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
    - loon.loonGrob
    - loon.grid.loon
    - loon.condGrob
    - loon.l_createCompoundGrob
    - loon.l_get_arrangeGrobArgs
    - loon.loonGrob_layoutType
  # Data
    desc: data
    contents:
    - loon.UsAndThem
    - loon.olive
    - loon.oliveAcids
    - loon.oliveLocations
    - loon.minority
  # Inspectors
    desc: inspectors
    contents:
    - ends_with("inspector")
    - loon.l_hist_inspector_analysis
    - loon.l_worldview
  # Others
    desc: others
    contents:
    - loon.l_list2nestedTclList
    - loon.l_nestedTclList2list
    - loon.l_data
    - loon.l_throwErrorIfNotLoonWidget
    - loon.l_toR
    - loon.l_isLoonWidget
    - loon.L2_distance
  # Measures
    desc: measures
    contents:
    - loon.measures1d
    - loon.measures2d
    - loon.scagnostics2d
    - loon.print.measures1d
    - loon.print.measures2d

'''