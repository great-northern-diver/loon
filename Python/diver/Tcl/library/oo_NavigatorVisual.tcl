


::oo::class create ::loon::classes::NavigatorVisual {

    superclass ::loon::classes::Visual

    variable navigatorObj navigatorId ids color_var label_var

    constructor {NavigatorId NavigatorObj args} {
	
	set navigatorId $NavigatorId
	set navigatorObj $NavigatorObj
	
	set color_var [uplevel #0 [list ${navigatorObj}::my varname color]]
	set label_var [uplevel #0 [list ${navigatorObj}::my varname label]]

	next {*}${args}	
    }

    method redraw {} {
	my variable visualid canvas
	## GraphViusal deletes the navigators (same visualid)
	
	## puts "Navigator $navigatorId, $navigatorObj"
	## pastell yellow

	set idEdge [uplevel #0 [list $canvas create line 0 0 0 0\
				    -fill [set $color_var] -width 3\
				    -tag [list layer $visualid navigatorEdge $navigatorId]]]
	
	set idProg [uplevel #0 [list $canvas create line 0 0 0 0\
				    -fill [set $color_var] -width 8\
				    -tag [list layer $visualid navigatorProgression $navigatorId]]]
	set idPathTerm [uplevel #0 [list $canvas create oval 0 0 0 0\
					-fill [set $color_var]\
					-tag [list layer $visualid navigatorPathEnd $navigatorId]]]
	
	set idNav [uplevel #0 [list $canvas create oval 0 0 0 0\
				   -fill [set $color_var]\
				   -tag [list layer $visualid navigator $navigatorId]]]
	
	set idLabel [uplevel #0 [list $canvas create text 0 0\
				     -text [set $label_var]\
				     -tag [list layer $visualid navigatorLabel $navigatorId]]]
	
	set ids [list $idEdge $idProg $idPathTerm $idNav $idLabel]	
    }
    
    method updateCoords {x y pathCoords seenCoords} {
	my variable canvas
	## the graph visual moves the bullet into place
	
	uplevel #0 [list $canvas coords [lindex $ids 0] {*}$pathCoords]
	
	uplevel #0 [list $canvas coords [lindex $ids 1] {*}$seenCoords]
	
	set xp [lindex $pathCoords end-1]
	set yp [lindex $pathCoords end]
	
	uplevel #0 [list $canvas coords [lindex $ids 2]\
			[expr {$xp - 4}] [expr {$yp - 4}]\
			[expr {$xp + 4}] [expr {$yp + 4}]]
	
	uplevel #0 [list $canvas coords [lindex $ids 3]\
			[expr {$x - 20}] [expr {$y - 20}]\
			[expr {$x + 20}] [expr {$y + 20}]]
	
	uplevel #0 [list $canvas coords [lindex $ids 4]\
			$x $y]
	
    }
    
    method recolor {} {
	my variable canvas
	foreach id [lrange $ids 0 3] {
	    uplevel #0 [list $canvas itemconfigure $id -fill [set $color_var]]
	}
	
    }

    method relabel {} {
	my variable canvas
	uplevel #0 [list $canvas itemconfigure [lindex $ids 4] -text [set $label_var]]
    }
    
}
