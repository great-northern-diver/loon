
oo::class create loon::classes::PointrangeGlyphVisual {
    
    superclass  ::loon::classes::GlyphVisual
    
    variable ymin_var ymax_var linewidth_var y_var swap_var
    
    constructor {glyphObj map xvar yvar args} {
	
	next $glyphObj $map $xvar $yvar {*}$args
	
	set ns [info object namespace $glyphObj]
	
 	set ymin_var [uplevel #0 [list ${ns}::my varname ymin]]
	set ymax_var [uplevel #0 [list ${ns}::my varname ymax]]
	set linewidth_var [uplevel #0 [list ${ns}::my varname linewidth]]
	
	set swap_var [uplevel #0 [list [info object namespace $map]::my varname swap]]
	set y_var $yvar
		
    }
    
    
    method draw {canvas ind} {
	set ids {}
	
	lappend ids [$canvas create oval 0 0 0 0]

	lappend ids [$canvas create line 0 0 0 0\
			 -width [lindex [set $linewidth_var] $ind]]
	
	return $ids
    }
    
    method updateCoords {ids canvas ind size} {
	
	my variable map
	
	## since orientation now matters for drawing y
	## is inverted
	set y [lindex [set $y_var] $ind]
	
	set swap [set $swap_var]
	
	set ymin_data [lindex [set $ymin_var] $ind]
	set ymax_data [lindex [set $ymax_var] $ind]
	
	set ymin [$map dPan2dS 0 [expr {$ymin_data - $y}]]
	set ymax [$map dPan2dS 0 [expr {$ymax_data - $y}]]

	#if {$swap} {
	#    set ymin [$map mapDy2S [expr {$ymin_data - $y}]]
	#    set ymax [$map mapDy2S [expr {$ymax_data - $y}]]
	#} else {
	#    set ymin [$map mapDy2S [expr {$y - $ymin_data}]]
	#    set ymax [$map mapDy2S [expr {$y - $ymax_data}]]
	#}
	

	
	if {$swap} {
	    $canvas coords [lindex $ids 1] [lindex $ymin 0] 0 [lindex $ymax 0] 0
	} else {
	    $canvas coords [lindex $ids 1] 0 [lindex $ymin 1] 0 [lindex $ymax 1]
	}
	
	set r [::loon::map_circle_size $size]
	$canvas coords [lindex $ids 0] -$r -$r $r $r
	
    }

    method recolor {ids canvas ind color} {
	my variable glyphObject
	
	if {[set ${glyphObject}::showArea]} {
	    $canvas itemconfigure [lindex $ids 0] -fill "" -outline $color
	} else {
	    $canvas itemconfigure [lindex $ids 0] -fill $color -outline $color
	}
	$canvas itemconfigure [lindex $ids 1] -fill $color
    }

}
