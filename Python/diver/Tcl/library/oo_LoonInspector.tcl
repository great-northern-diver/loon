oo::class create loon::classes::LoonInspector {

    superclass ::loon::classes::Inspector2
    
    variable plot plot3D serialaxes graph histogram packedinspector validWidgetTypes

    constructor {Path} {
	set validWidgetTypes {Serialaxes_Widget Scatterplot_Widget Graph_Widget Histogram_Widget Scatterplot3D_Widget}
	next $Path
    }
    
    method Make {} {
	my variable path

	frame $path -class LoonInspector
	
	foreach w {plot plot3D histogram graph serialaxes} {
	    set $w [::loon::${w}_inspector ${path}.$w]
	}
	pack $plot -side top -anchor nw -fill x -expand TRUE
	set packedinspector plot
    }

    
    method CheckNewActivewidget {widget} {
	set class [regsub "::loon::classes::"\
		       [info object class $widget] ""]
	
	if {$class ni $validWidgetTypes} {
	    error "$widget is not a Histogram, Scatterplot, Scatterplot3D, Serialaxes, or Graph Widget."
	}
    }
    
    method RegisterActivewidget {} {
	my variable activewidget
	
	## update inspector
	if {$activewidget eq ""} {
	    set type ""
	} else {
	    set class [regsub "::loon::classes::"\
			   [info object class $activewidget] ""]
	    
	    switch -- $class {
		Histogram_Widget {
		    set type histogram
		}
		Scatterplot_Widget {
		    set type plot
        }
        Scatterplot3D_Widget {
            set type plot3D
		}
		Graph_Widget {
		    set type graph
		}
		Serialaxes_Widget {
		    set type serialaxes
		}
		default {
		    set type ""
		}
	    }
	}
	
	if {$type eq ""} {
	    [set $packedinspector] configure -activewidget ""
	} else {
	    if {$type ne $packedinspector} {
		pack forget [set $packedinspector]
		[set $packedinspector] configure -activewidget ""
		
		pack [set $type] -side top -anchor nw -fill x -expand TRUE
		set packedinspector $type
	    }
	    
	    [set $type] configure -activewidget $activewidget
	}
    }

    

}
