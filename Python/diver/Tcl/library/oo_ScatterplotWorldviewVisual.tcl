::oo::class create ::loon::classes::ScatterplotWorldviewVisual { 
    
    superclass ::loon::classes::ScatterplotVisual

    ## the Scatterplot Worldview Visual is special because
    ## - shows the points always with the same size
    ## - adds a boundingbox around the data



    method MakeStateBinding {} {
	next FALSE
    }
    
    method LayerUpdateDict {events} {
	if {[dict exists $events "active"]} {
	    my updateActive
	}
	
	if {[dict exists $events "color"]||\
		[dict exists $events "selected"]} {
	    my recolor
	}	
    }

    
    method redraw {} {
	my variable canvas visualid isVisible ids n_var tag_var
	
	if {$ids ne "noinit"} {
	    my clear
	} 
	
	if {$isVisible} { set state normal } else { set state hidden }
	
	set tag [list layer $visualid]
	
	if {[set $n_var] eq 0} {
	    set ids [uplevel #0 [list $canvas create oval\
				     0 0 0 0 -tag $tag -state hidden]]
	} else {
	    set ids {}
	    set i 0
	    foreach t [set $tag_var] {
		lappend ids [uplevel #0 [list $canvas create oval 0 0 0 0\
					     -tag [concat $tag "point$i" $t] -state $state]]
		incr i
	    }
	    
	    my updateCoords
	    my recolor
	    my updateActive
	}
	
	nextto ::loon::classes::LayerVisual ;## move layer into correct place
    }



    method updateCoords {} {
	my variable canvas map isVisible x_var y_var xTemp_var yTemp_var\
	    size_var n_var ids curX curY

	if {[llength [set $x_var]] ne [llength $ids]} {
	    my redraw
	    return
	}
	
	if {$isVisible && [set $n_var] ne 0} {
	    
	    if {[llength [set $xTemp_var]] ne 0} {
		set xvar $xTemp_var
	    } else {
		set xvar $x_var
	    }
	    if {[llength [set $yTemp_var]] ne 0} {
		set yvar $yTemp_var
	    } else {
		set yvar $y_var
	    }
	    
	    
	    set sxycoords [$map mapDxy2Sxy [set $xvar] [set $yvar]]
	    
	    set curX [lindex $sxycoords 0]
	    set curY [lindex $sxycoords 1]

	    
	    foreach id $ids\
		x $curX y $curY size [set $size_var] {
		    if {$size < 1} {
		    	set r 1
		    } else {
			set r 2
		    }
		    
		    uplevel #0 [list $canvas coords $id -$r -$r $r $r]
		    uplevel #0 [list $canvas move $id $x $y]
		}
	}
    }    

    method recolor {} {
	my variable isVisible canvas n_var ids color_var selected_var

	if {$isVisible && [set $n_var] ne 0} {	    
	    set selColor $::loon::Options(select-color)
	    foreach id $ids color [set $color_var] selected [set $selected_var] {
		if {$selected} {
		    uplevel #0 [list $canvas itemconfigure $id\
				    -fill $selColor -outline $selColor]
		} else {
		    uplevel #0 [list $canvas itemconfigure $id\
				    -fill $color -outline $color]
		}
	    }
	}
    }
    

}
