

## A Single Oval
::oo::class create ::loon::classes::OvalVisual { 
    
    superclass ::loon::classes::PolygonVisual
    
    method redraw {} {
	my variable canvas isVisible id
	
	if {$id ne "noinit"} {
	    my clear
	}
	
	if {$isVisible} {set state normal} else {set state hidden}
	
	set id [uplevel #0 [list $canvas create oval 0 0 0 0\
				-state $state]]
	
	my updateCoords
	my updateItem
	
	nextto ::loon::classes::LayerVisual ;## move layer into correct place
    }
    
}
