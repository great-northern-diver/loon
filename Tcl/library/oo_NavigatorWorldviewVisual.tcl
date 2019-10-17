
::oo::class create ::loon::classes::NavigatorWorldviewVisual {
    
    superclass ::loon::classes::NavigatorVisual

    method redraw {} {
	my variable visualid canvas color_var navigatorId ids
	
	set idEdge [uplevel #0 [list $canvas create line 0 0 0 0\
				    -fill [set $color_var] -width 2\
				    -tag [list layer $visualid navigatorEdge $navigatorId]]]
	
	set idProg [uplevel #0 [list $canvas create line 0 0 0 0\
				    -fill [set $color_var] -width 4\
				    -tag [list layer $visualid navigatorProgression $navigatorId]]]
	
	set idNav [uplevel #0 [list $canvas create oval 0 0 0 0\
				   -fill [set $color_var]\
				   -tag [list layer $visualid navigator $navigatorId]]]
	
	set ids [list $idEdge $idProg $idNav]	
    }

    
    method updateCoords {x y pathCoords seenCoords} {
	my variable canvas ids
	## the graph visual moves the bullet into place

	uplevel #0 [list $canvas coords [lindex $ids 0] {*}$pathCoords]
	
	uplevel #0 [list $canvas coords [lindex $ids 1] {*}$seenCoords]

	if {[::loon::listfns::isNumeric [list $x $y]]} {
	    uplevel #0 [list $canvas coords [lindex $ids 2]\
			    [expr {$x - 5}] [expr {$y - 5}]\
			    [expr {$x + 5}] [expr {$y + 5}]]
       	} else {
	    puts "warning navigator worldview visual x=$x and y=$y"
	}
    }

}
