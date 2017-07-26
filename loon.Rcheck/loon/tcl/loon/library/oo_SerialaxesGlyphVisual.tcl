
oo::class create loon::classes::SerialaxesGlyphVisual {
    
    superclass  ::loon::classes::GlyphVisual
    
    variable angles xdirs ydirs
    
    constructor {args} {
	set angles {}
	set xdirs {}
	set ydirs {}

	next {*}$args
	
    }
    
    method draw {canvas ind} {
	my variable glyphObject
	
	set p [llength [set ${glyphObject}::sequence]]

	if {$p eq 0} {return -1}
	
	if {[set ${glyphObject}::axesLayout] eq "radial"} {
	    set ids [my DrawStar $p $canvas $ind]
	} else {
	    ## parallel coordinates
	    set ids [my DrawTrajectory $p $canvas $ind]
	}
	
	return $ids
    }
    
    
    method TrajectoryCoords {glyph s h} {
	set coords {}
	
	set p [llength $glyph]
	set x [expr {-1.0*($p-1)/2.0*$s}]
	
	foreach g $glyph {
	    lappend coords $x [expr {(1-double($g))*$h-$h/2.0}]
	    set x [expr {$x+$s}]
	}
	
	return $coords
    }

    ## This Fucntion creates only the canvas items...
    method DrawTrajectory {p canvas ind} {
	my variable glyphObject
	
	set ids {}
       	
	## Create bounding box
	if {[set ${glyphObject}::showEnclosing]} {
	    lappend ids [uplevel #0 [list $canvas create rect 0 0 0 0\
					 -fill ""\
					 -outline [set ${glyphObject}::bboxColor]]]
	}
	
	## Create Axes
	if {[set ${glyphObject}::showAxes]} {
	    set axesCol [set ${glyphObject}::axesColor]
	    for {set i 1} {$i < $p} {incr i} {
		lappend ids [uplevel #0 [list $canvas create line 0 0 0 0\
					     -fill $axesCol]]
	    }
	} 
	
	## Draw Trajectory
	if {[set ${glyphObject}::showArea]} {
	    lappend ids [uplevel #0 [list $canvas create polygon 0 0 0 0]]
	} else {
	    set linewidth [lindex [set ${glyphObject}::linewidth] $ind]
	    lappend ids [uplevel #0 [list $canvas create line 0 0 0 0\
					 -width $linewidth]]
	}
	
	return $ids
    } 

    
    ## angles and directions
    method UpdateAngles {p} {
	set angles {}; set xdirs {}; set ydirs {}
	if {$p > 0} { 
	    for {set i 0} {$i < $p} {incr i} {
		set angle [expr {2*$::loon::pi*(1-double($i)/$p)}]
		lappend angles $angle
		lappend xdirs [expr {double(cos($angle))}]
		lappend ydirs [expr {double(sin($angle))}]
	    }
	}	 
    }
    
    method StarCoords {glyph radius {inverse FALSE}} {
	set coords {}
	foreach g $glyph xdir $xdirs ydir $ydirs {
	    if {$inverse} {
		lappend coords [expr {$radius * (1 - $g) * $xdir}]
		lappend coords [expr {$radius * (1 - $g) * $ydir}]
	    } else {
		lappend coords [expr {$radius * $g * $xdir}]
		lappend coords [expr {$radius * $g * $ydir}]		
	    }
	}
	return $coords
    }
    
    
    method DrawStar {p canvas ind} {
	my variable glyphObject
	
	if {[llength $angles] ne $p} {
	    my UpdateAngles $p
	}
	
	set ids {}
	
	if {[set ${glyphObject}::showEnclosing]} {
	    lappend ids [uplevel #0 [list $canvas create oval 0 0 0 0\
					 -fill ""\
					 -outline [set ${glyphObject}::bboxColor]]]
	}
	
	set linewidth [lindex [set ${glyphObject}::linewidth] $ind]

	if {[set ${glyphObject}::showArea]} {
	    lappend ids [uplevel #0 [list $canvas create polygon 0 0 0 0 -width $linewidth]]
	    # Axes 	
	    if {[set ${glyphObject}::showAxes]} {
		set axesCol [set ${glyphObject}::axesColor]
		for {set i 0} {$i < $p} {incr i} {
		    lappend ids [uplevel #0 [list $canvas create line 0 0 0 0\
						    -fill $axesCol -width 1]]
		}	    
	    }   
	} else {
	    ## if showArea then axes are below star
	    set idaxes {}
	    # Axes 	
	    if {[set ${glyphObject}::showAxes]} {
		set axesCol [set ${glyphObject}::axesColor]
		for {set i 0} {$i < $p} {incr i} {
		    lappend idaxes [uplevel #0 [list $canvas create line 0 0 0 0\
						    -fill $axesCol -width 1]]
		}	    
	    }       
	    lappend ids [uplevel #0 [list $canvas create line 0 0 0 0 -width $linewidth]]
	    lappend ids {*}$idaxes
	}
	
	
	return $ids
    }
    
    
    method updateCoords {ids canvas ind size} {
	my variable glyphObject
	
	if {[set ${glyphObject}::axesLayout] eq "radial"} {
	
	    set radius [::loon::map_star_size $size]
	    
	    set nextid 0
	    if {[set ${glyphObject}::showEnclosing]} {
		uplevel #0 [list $canvas coords [lindex $ids 0]\
				-$radius -$radius $radius $radius]
		incr nextid
	    }
	    
	    ## Star
	    set coords [my StarCoords\
			    [lindex [set ${glyphObject}::glyphs] $ind]\
			    $radius]
	
	    ## update glyph
	    if {[set ${glyphObject}::showArea]} {
		uplevel #0 [list $canvas coords [lindex $ids $nextid] $coords]
	    } else {
		## uses lines, hence need to close shape
		uplevel #0 [list $canvas coords [lindex $ids $nextid]\
				[concat $coords {*}[lrange $coords 0 1]]] 
	    }
	    incr nextid
	
	    
	    ## Axes
	    if {[set ${glyphObject}::showAxes]} {
		## if enclosing is shown draw axes out to enclosing
		## otherwise only as far as the star goes
		if {[set ${glyphObject}::showEnclosing]} {
		    foreach xdir $xdirs\
			ydir $ydirs\
			id [lrange $ids $nextid end] {
			    uplevel #0 [list $canvas coords $id 0 0\
					    [expr {$radius * $xdir}]\
					    [expr {$radius * $ydir}]]
			}
		} else {
		    foreach {xstar ystar} [concat $coords {*}[lrange $coords 0 1]]\
			id [lrange $ids $nextid end] {
			    uplevel #0 [list $canvas coords $id 0 0 $xstar $ystar]
			}
		}
	    }
	    
	} else {
	    
	    set p [llength [set ${glyphObject}::sequence]]

	    ## axes-distance
	    set s [::loon::map_pc_size $size]
	    
	    set w2 [expr {($p-1)/2.0*double($s)}]
	    set h [expr {4.0*$s}]
	    set h2 [expr {$h/2.0}]

	    set nextid 0
	    if {[set ${glyphObject}::showEnclosing]} {
		uplevel #0 [list $canvas coords [lindex $ids 0] -$w2 -$h2 $w2 $h2]
		incr nextid
	    }
	    
		
	    if {[set ${glyphObject}::showAxes]} {
		set x [expr {-$w2+$s}]
		foreach id [lrange $ids $nextid end-1] {
		    uplevel #0 [list $canvas coords $id $x -$h2 $x $h2]
		    set x [expr {$x+$s}]
		}
	    } 
	    
	    
	    ## Update Trajectory
	    set coords [my TrajectoryCoords\
			    [lindex [set ${glyphObject}::glyphs] $ind]\
			    $s $h]
	    
	    if {[set ${glyphObject}::showArea]} {
		set coords [concat -$w2 $h2 $coords $w2 $h2]
	    } 
	    uplevel #0 [list $canvas coords [lindex $ids end] $coords]	    
	}
	
    }
    
    method recolor {ids canvas ind color} {
	my variable glyphObject
	
	if {[set ${glyphObject}::axesLayout] eq "radial"} {

	    if {[set ${glyphObject}::showEnclosing]} {
		set id [lindex $ids 1]
	    } else {
		set id [lindex $ids 0]
	    }
	    
	    if {[set ${glyphObject}::showArea]} {
		uplevel #0 [list $canvas itemconfigure $id -fill $color -outline $color]
	    } else {
		uplevel #0 [list $canvas itemconfigure $id -fill $color]
	    }
	    
	} else {
	    set id [lindex $ids end]
	    uplevel #0 [list $canvas itemconfigure $id -fill $color]	    
	}
	
	
	
    }
	
	
    
}
