
namespace eval loon {

    namespace export plot
    namespace export plot3D
    namespace export plot_inspector
    namespace export plot3D_inspector
    namespace export worldview
    namespace export plot_inspector_analysis
    namespace export layers_inspector
    namespace export glyphs_inspector
    namespace export graph
    namespace export graph_inspector
    namespace export graph_inspector_analysis

    namespace export ladder
    namespace export colorcrosstable
    namespace export graphswitch

    namespace export serialaxes
    namespace export serialaxes_inspector
    namespace export loon_inspector

    namespace export serialaxes_glyph_inspector
    namespace export pointrange_glyph_inspector
    namespace export text_glyph_inspector
    namespace export image_glyph_inspector
    namespace export spiro_glyph_inspector
    namespace export polygon_glyph_inspector

    namespace export navigators_inspector

    namespace export histogram
    namespace export histogram_inspector
    namespace export histogram_inspector_analysis

    namespace export minmax_scale



    proc plot {args} {
	set widget [WidgetFactory Scatterplot plot {*}$args]
	return  $widget
    }

    proc plot3D {args} {
	set widget [WidgetFactory Scatterplot3D plot3D {*}$args]
	return  $widget
    }

    proc plot_inspector {args} {
	set widget [WidgetFactory PlotInspector plotInspector {*}$args]
	return  $widget
    }

    proc plot3D_inspector {args} {
	set widget [WidgetFactory Plot3DInspector plot3DInspector {*}$args]
	return  $widget
    }

    proc worldview {args} {
	set widget [WidgetFactory Worldview worldview {*}$args]
	return  $widget
    }

    proc plot_inspector_analysis {args} {
	set widget [WidgetFactory PlotInspectorAnalysis plotInspectorAnalysis {*}$args]
	return  $widget
    }

    proc plot3D_inspector_analysis {args} {
	set widget [WidgetFactory Plot3DInspectorAnalysis plotInspectorAnalysis {*}$args]
	return  $widget
    }

    proc layers_inspector {args} {
	set widget [WidgetFactory LayersInspector layersInspector {*}$args]
	return  $widget
    }

    proc glyphs_inspector {args} {
	set widget [WidgetFactory GlyphsInspector glyphsInspector {*}$args]
	return  $widget
    }

    proc histogram {args} {
	set widget [WidgetFactory Histogram hist {*}$args]
	return  $widget
    }

    proc histogram_inspector {args} {
	set widget [WidgetFactory HistogramInspector histogramInspector {*}$args]
	return  $widget
    }

    proc histogram_inspector_analysis {args} {
	set widget [WidgetFactory HistogramInspectorAnalysis histogramInspectorAnalysis {*}$args]
	return  $widget
    }


    proc graph {args} {
	set widget [WidgetFactory Graph graph {*}$args]
	#update; ## important so that all bindings get sent
	return  $widget
    }

    proc graph_inspector {args} {
	set widget [WidgetFactory GraphInspector graphInspector {*}$args]
	return  $widget
    }

    proc graph_inspector {args} {
	set widget [WidgetFactory GraphInspector graphInspector {*}$args]
	return  $widget
    }

    proc graph_inspector_analysis {args} {
	set widget [WidgetFactory GraphInspectorAnalysis graphInspectorAnalysis {*}$args]
	return  $widget
    }

    proc ladder {args} {
	return [WidgetFactory Ladder ladder {*}$args]
    }

    proc colorcrosstable {args} {
	return [WidgetFactory ColorCrossTable colorcrosstable {*}$args]
    }

    proc graphswitch {args} {
	return [WidgetFactory Graphswitch graphswitch {*}$args]
    }

    proc serialaxes {args} {
	return [WidgetFactory Serialaxes serialaxes {*}$args]
    }

    # Once the layers are allowed, consider uncomment it.
    #proc serialaxes_inspector {args} {
	#set widget [WidgetFactory\
	#		SerialaxesInspector\
	#		serialaxesInspector\
	#		{*}$args]
	#return  $widget
    #}

    #proc serialaxes_inspector_analysis {args} {
	#set widget [WidgetFactory\
	#		SerialaxesInspectorAnalysis\
	#		serialaxesInspectorAnalysis\
	#		{*}$args]
	#return  $widget
    #}

    proc serialaxes_inspector {args} {
	set widget [WidgetFactory\
			SerialaxesInspectorAnalysis\
			serialaxesInspectorAnalysis\
			{*}$args]
	return  $widget
    }


    proc loon_inspector {args} {
	variable loonInspector

	if {[llength $loonInspector] eq 0} {
	    set loonInspector [WidgetFactory\
				   LoonInspector\
				   looninspector\
				   {*}$args]
	    bind $loonInspector <Destroy> "+set ::loon::loonInspector {}"
	} else {
	    error "loon inpsector is a singleton and is already open with\
                   widget path name $loonInspector"
	}

	return  $loonInspector
    }


    proc minmax_scale {args} {
	return [WidgetFactory MinMaxScale minmaxscale {*}$args]
    }



    foreach cmd {serialaxes_glyph_inspector\
		     pointrange_glyph_inspector\
		     text_glyph_inspector\
		     image_glyph_inspector\
		     navigators_inspector\
		     spiro_glyph_inspector\
		     polygon_glyph_inspector} {

	set class [join [lmap str [split $cmd "_"] {string toupper $str 0}] ""]

	eval [list proc $cmd "{args}"\
		  "return \[WidgetFactory $class [string tolower $class 0] {*}\$args\]"]

    }



}

