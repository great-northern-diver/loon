
::oo::class create ::loon::classes::PointsVisual { 
    
    superclass ::loon::classes::LayerVisualN
    
    variable size_var color_var linecolor_var linewidth_var
    
    constructor {Layerobj args} {
	
	set ns [info object namespace $Layerobj]	
	foreach state {color size linecolor linewidth} {
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
	    lappend ids [uplevel #0 [list $canvas create oval 0 0 0 0]]
	}
	
	my updateActive
	my updateCoords
	my updateItems
	
	next ;## move layer into correct place
    }
    
    method updateCoords {} {
	my variable canvas map isVisible ids n_var x_var y_var

	if {$ids eq "noinit" || [llength $ids] ne [set $n_var]} {
	    my redraw
	} else {
	    if {$isVisible} {
		set sxycoords [$map mapDxy2Sxy [set $x_var] [set $y_var]]
		
		foreach id $ids\
		    x [lindex $sxycoords 0] y [lindex $sxycoords 1]\
		    r [::loon::map_circle_size [set $size_var]] {
			uplevel #0 [list $canvas coords $id -$r -$r $r $r]
			uplevel #0 [list $canvas move $id $x $y]
		    }
	    }
	}
    }
 
    method updateZoomPan {oldPanX oldPanY oldZoomX oldZoomY} {
	my variable isVisible canvas map ids x_var y_var

	if {$isVisible} {
	    set diff [$map dPanZoom2dS [set $x_var] [set $y_var]\
			  $oldPanX $oldPanY $oldZoomX $oldZoomY]
	    
	    foreach id $ids dx [lindex $diff 0] dy [lindex $diff 1] {
		uplevel #0 [list $canvas move $id $dx $dy]
	    }
	    
	}
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
