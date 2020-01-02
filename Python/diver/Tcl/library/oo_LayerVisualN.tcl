

::oo::class create ::loon::classes::LayerVisualN {

    superclass ::loon::classes::LayerVisual

    variable ids n_var active_var type_var

    constructor {Layerobj args} {

	set ids "noinit"

	set ns [info object namespace $Layerobj]	
	set n_var [uplevel #0 [list ${ns}::my varname n]]
	set active_var [uplevel #0 [list ${ns}::my varname active]]
	set type_var [uplevel #0 [list ${ns}::my varname type]]
	
	next $Layerobj {*}$args	
    }
    
    method getTags {} {
	my variable visualid tag_var

	set type [set $type_var]
	set tags {}
	
	set i 0
	foreach tag [set $tag_var] {
	    lappend tags [list layer $visualid $type item$i $tag]
	    incr i
	}	
	return $tags
    }

    method layerupdate {events} {
	if {"n" in $events} {
	    my redraw
	} else {
	    if { "x" in $events || "y" in $events} {
		my updateCoords
	    }
	    if {"active" in $events} {
		my updateActive
	    }
	    my updateItems
	}
    }
    
    method updateItems {} {}

    method updateCoords {} {
	my variable map canvas isVisible x_var y_var

	if {$ids eq "noinit"} {
	    my redraw
	} else {
	    if {$isVisible} {
		foreach id $ids x [set $x_var] y [set $y_var] {
		    uplevel #0 [list $canvas coords $id\
				    [$map mapDxy2Scoords $x $y]]
		}
	    }
	}
    }

    method updateActive {} {
	my variable canvas isVisible
	
	if {$ids eq "noinit"} {
	    my redraw
	} else {
	    foreach id $ids active [set $active_var] {
		if {$active && $isVisible} {
		    set state normal
		} else {
		    set state hidden
		}
		uplevel #0 [list $canvas itemconfigure $id -state $state]
	    }
	}
    }
    

    
    method clear {} {
	next
	set ids "noinit"	
    }


    
}
