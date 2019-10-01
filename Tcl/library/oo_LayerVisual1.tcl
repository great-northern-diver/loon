

::oo::class create ::loon::classes::LayerVisual1 {

    superclass ::loon::classes::LayerVisual

    variable id type_var

    constructor {Layerobj args} {
	set id "noinit"

	set ns [info object namespace $Layerobj]
	set type_var [uplevel #0 [list ${ns}::my varname type]]
	
	next $Layerobj {*}$args	
    }
    
    method getTags {} {
	my variable visualid tag_var

	set type [set $type_var]
	
	return [concat layer $visualid $type item0 {*}[set $tag_var]]
    }
    method updateItem {} {}
    
    ## Called by state binding
    method layerupdate {events} {
	if { "x" in $events || "y" in $events} {
	    my updateCoords
	}	
	my updateItem
    }

    method updateCoords {} {
	my variable canvas map isVisible x_var y_var
	
	if {$id eq "noinit"} {
	    my redraw
	} else {	
	    if {$isVisible} {
		uplevel #0 [list $canvas coords $id\
				[$map mapDxy2Scoords [set $x_var] [set $y_var]]]
	    }
	}
    }

    method clear {} {
	next
	set id "noinit"	
    }
    
}
