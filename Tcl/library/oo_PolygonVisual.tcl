

## A Single Polygon
::oo::class create ::loon::classes::PolygonVisual { 
    
    superclass ::loon::classes::LayerVisual1

    variable color_var linecolor_var linewidth_var

    constructor {Layerobj args} {
	
	set ns [info object namespace $Layerobj]
	foreach state {color linewidth linecolor} {
	    set ${state}_var [uplevel #0 [list ${ns}::my varname $state]]
	}
	
	next $Layerobj {*}$args
	
    }
    
    method redraw {} {
	my variable canvas isVisible id
	
	if {$id ne "noinit"} {
	    my clear
	}
	
	if {$isVisible} {set state normal} else {set state hidden}
	
	set id [uplevel #0 [list $canvas create polygon 0 0 0 0\
				-state $state]]
	
	my updateCoords
	my updateItem
	
	next ;## move layer into correct place	
    }
    
    method updateItem {} {
	my variable isVisible canvas id
	
	if {$id eq "noinit"} {
	    my redraw
	} else {	
	    if {$isVisible} {
		uplevel #0 [list $canvas itemconfigure $id\
				-fill [set $color_var]\
				-width [set $linewidth_var]\
				-outline [set $linecolor_var]\
				-tag [my getTags]]
	    }
	}
    }

    
}
