

## N rectangles
::oo::class create ::loon::classes::RectanglesVisual { 

    ## same color, linecolor and linewidth states
    superclass ::loon::classes::PolygonsVisual
    
    method redraw {} {
	my variable canvas ids n_var
	
	if {$ids ne "noinit"} {
	    my clear
	}
	
	set ids {}
	for {set i 0} {$i < [set $n_var]} {incr i} { 
	    lappend ids [uplevel #0 [list $canvas create rect {0 0 0 0}]]
	}
	
	my updateActive
	my updateCoords
	my updateItems
	
	nextto ::loon::classes::LayerVisual ;## move layer into correct place
    }
    
}
