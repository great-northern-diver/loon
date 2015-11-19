
::oo::class create ::loon::classes::PointsWorldviewVisual { 
    
    superclass ::loon::classes::PointsVisual
    
    method updateCoords {} {
	my variable canvas map isVisible ids n_var x_var y_var
	
	if {$ids eq "noinit" || [llength $ids] ne [set $n_var]} {
	    my redraw
	} else {
	    if {$isVisible} {
		set sxycoords [$map mapDxy2Sxy [set $x_var] [set $y_var]]
		
		foreach id $ids\
		    x [lindex $sxycoords 0] y [lindex $sxycoords 1] {
			uplevel #0 [list $canvas coords $id -1 -1 1 1]
			uplevel #0 [list $canvas move $id $x $y]
		    }
	    }
	}
    }

	
}
