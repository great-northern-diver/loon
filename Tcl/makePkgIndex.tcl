#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

## Make the pkgIndex.tcl file for the loon package

set files [glob -directory library *.tcl]

#  Slurp up the data file
set fp [open "pkgIndex.tcl" w+]

#puts $fp "if \{\[catch \{package require Img\}\]\} \{
#    puts \"Img package is missing. Can not load png and jpeg images.\"
#\}"

puts  $fp "package ifneeded loon 1.2.3 \""
puts  $fp "   package provide loon 1.2.3 "
puts  $fp "   package require Tcl 8.6 "
puts  $fp "   package require Tk 8.6 "
## Objects need to be loaded according to inheritance

set oofiles [list\
listfns.tcl\
hcl.tcl\
optionDatabase.tcl\
oo_fancyInfo.tcl\
oo_Configurable.tcl\
oo_Configurable_Check.tcl\
oo_Bindable.tcl\
oo_withStateBindings.tcl\
oo_withCanvasAndItemBindings.tcl\
oo_VariableDimensions.tcl\
oo_Linkable.tcl\
oo_withLayers.tcl\
oo_LabelledCollection.tcl\
oo_withItemLabels.tcl\
oo_withGlyphs.tcl\
oo_Plot_Model.tcl\
oo_XYPair_Model.tcl\
oo_Scatterplot_Model.tcl\
oo_Bindings.tcl\
oo_StateBindings.tcl\
oo_CollectionBindings.tcl\
oo_LayerBindings.tcl\
oo_ItemBindings.tcl\
oo_CanvasBindings.tcl\
oo_Layer.tcl\
oo_GroupLayer.tcl\
oo_PrimitiveLayer.tcl\
oo_PrimitiveLayer1.tcl\
oo_PrimitiveLayerN.tcl\
oo_PolygonLayer.tcl\
oo_PolygonsLayer.tcl\
oo_RectangleLayer.tcl\
oo_RectanglesLayer.tcl\
oo_OvalLayer.tcl\
oo_PointsLayer.tcl\
oo_TextLayer.tcl\
oo_TextsLayer.tcl\
oo_LineLayer.tcl\
oo_LinesLayer.tcl\
oo_ModelLayer.tcl\
oo_ScatterplotLayer.tcl\
oo_GraphLayer.tcl\
oo_Visual.tcl\
oo_BoxVisual.tcl\
oo_LabelsVisual.tcl\
oo_BorderVisual.tcl\
oo_ScalesGuidesVisual.tcl\
oo_LayerVisual.tcl\
oo_LayerVisual1.tcl\
oo_LayerVisualN.tcl\
oo_PolygonVisual.tcl\
oo_PolygonsVisual.tcl\
oo_RectangleVisual.tcl\
oo_RectanglesVisual.tcl\
oo_OvalVisual.tcl\
oo_TextVisual.tcl\
oo_PointsVisual.tcl\
oo_LineVisual.tcl\
oo_LinesVisual.tcl\
oo_TextsVisual.tcl\
oo_ScatterplotVisual.tcl\
oo_ScatterplotWorldviewVisual.tcl\
oo_PointsWorldviewVisual.tcl\
oo_GraphVisual.tcl\
oo_GraphWorldviewVisual.tcl\
oo_NavigatorVisual.tcl\
oo_NavigatorWorldviewVisual.tcl\
oo_GlyphVisual.tcl\
oo_ImageGlyphVisual.tcl\
oo_PointrangeGlyphVisual.tcl\
oo_SerialaxesGlyphVisual.tcl\
oo_SpiroGlyphVisual.tcl\
oo_PolygonGlyphVisual.tcl\
oo_TextGlyphVisual.tcl\
oo_colormenu.tcl\
oo_linkingGroupWidget.tcl\
oo_Inspector2.tcl\
oo_LayersInspector.tcl\
oo_PlotInspectorAnalysis.tcl\
oo_Worldview.tcl\
oo_PlotInspector.tcl\
oo_HistogramInspector.tcl\
oo_HistogramInspectorAnalysis.tcl\
oo_SerialaxesInspectorAnalysis.tcl\
oo_LoonInspector.tcl\
oo_Ladder.tcl\
oo_Graphswitch.tcl\
oo_withNavigators.tcl\
oo_withContexts.tcl\
oo_Navigator.tcl\
oo_Graph_Model.tcl\
oo_Map.tcl\
oo_PlotMap.tcl\
oo_WorldviewMap.tcl\
oo_Plot_View.tcl\
oo_NonLayered_View.tcl\
oo_Layered_View.tcl\
oo_Decorated_View.tcl\
oo_Scatterplot_View.tcl\
oo_Worldview_View.tcl\
oo_Graph_View.tcl\
oo_Controller.tcl\
oo_Canvas_Controller.tcl\
oo_Resize_Controller.tcl\
oo_ZoomPan_Controller.tcl\
oo_ButtonPressMotion_Controller.tcl\
oo_ItemLabel_Controller.tcl\
oo_SweepBrush_Controller.tcl\
oo_TempMove_Controller.tcl\
oo_Scatterplot_Controller.tcl\
oo_Worldview_Controller.tcl\
oo_Graph_Controller.tcl\
oo_Serialaxes_Controller.tcl\
oo_Widget.tcl\
oo_Plot_Widget.tcl\
oo_Scatterplot_Widget.tcl\
oo_Graph_Widget.tcl\
oo_Serialaxes.tcl\
oo_Serialaxes_Model.tcl\
oo_Serialaxes_View.tcl\
oo_SerialaxesAbstractVisual.tcl\
oo_StarglyphsStackedVisual.tcl\
oo_ParallelCoordinatesVisual.tcl\
oo_Glyph.tcl\
oo_TextGlyph.tcl\
oo_ImageGlyph.tcl\
oo_SerialaxesGlyph.tcl\
oo_SpiroGlyph.tcl\
oo_PolygonGlyph.tcl\
oo_PointrangeGlyph.tcl\
oo_GlyphInspector.tcl\
oo_SerialaxesGlyphInspector.tcl\
oo_SpiroGlyphInspector.tcl\
oo_PolygonGlyphInspector.tcl\
oo_PointrangeGlyphInspector.tcl\
oo_ImageGlyphInspector.tcl\
oo_TextGlyphInspector.tcl\
oo_GlyphsInspector.tcl\
oo_NavigatorsInspector.tcl\
oo_NavigatorInspector.tcl\
oo_withData.tcl\
oo_withScaledData.tcl\
oo_Context.tcl\
oo_Context2d.tcl\
oo_Geodesic2d.tcl\
oo_Slicing2d.tcl\
oo_Histogram_Model.tcl\
oo_Histogram_View.tcl\
oo_Histogram_Widget.tcl\
oo_Histogram_Controller.tcl\
oo_HistogramVisual.tcl\
oo_HistogramWorldviewVisual.tcl\
oo_HistogramLayer.tcl\
oo_ColorCrossTable.tcl\
oo_Rotate3D_Controller.tcl\
oo_Scatterplot3D_Model.tcl\
oo_Plot3DInspectorAnalysis.tcl\
oo_Scatterplot3D_Widget.tcl\
oo_Axes3DVisual.tcl\
oo_Scatterplot3D_View.tcl\
oo_Scatterplot3D_Controller.tcl\
oo_Plot3DInspector.tcl
]

## Environment Variables
puts $fp "   \[list set ::env(LOON_VERSION) 1.1.0\]"
puts $fp "   \[list set ::env(LOON_LIBRARY) \[file dirname \[info script\]\]\]"

## Init File
puts $fp "   \[list source \[file join \$dir library/init.tcl\]\]"

## Object Files
foreach file $oofiles {
    puts $fp "   \[list source \[file join \$dir library/$file\]\]"
}


## Remaining files
set omitfiles [concat $oofiles init.tcl]

foreach file $files {
    #    puts [file join \$dir $file]
    set rsfile [regsub "^library/" $file ""]

    if {$rsfile ni $omitfiles} {
	    puts $fp "   \[list source \[file join \$dir $file\]\]"
	}
}


puts $fp "\""
close $fp
