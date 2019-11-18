

## N Polygons
::oo::class create ::loon::classes::PolygonsVisual { 
    
    superclass ::loon::classes::LayerVisualN

    variable color_var linecolor_var linewidth_var
    
    constructor {Layerobj args} {
	
	set ns [info object namespace $Layerobj]
	foreach state {color linewidth linecolor} {
	    set ${state}_var [uplevel #0 [list ${ns}::my varname $state]]
	}

	next $Layerobj {*}$args
	
    }
    
    method redraw {} {
	my variable canvas ids n_var
	
	if {$ids ne "noinit"} {
	    my clear
	}
	
	set ids {}
	for {set i 0} {$i < [set $n_var]} {incr i} { 
	    lappend ids [uplevel #0 [list $canvas create polygon {}]]
	}
	
	my updateActive
	my updateCoords
	my updateItems
	
	next ;## move layer into correct place
    }
    
    method updateItems {} {
	my variable canvas ids
	
	if {$ids eq "noinit"} {
	    my redraw
	} else {
	    foreach id $ids\
		color [set $color_var]\
		linecolor [set $linecolor_var]\
		linewidth [set $linewidth_var]\
		tag [my getTags] {
		    uplevel #0 [list $canvas itemconfigure $id\
				    -fill $color\
				    -outline $linecolor\
				    -width $linewidth\
				    -tag $tag]
		}
	}
    }
}
