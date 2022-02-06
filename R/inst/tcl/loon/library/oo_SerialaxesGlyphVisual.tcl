
oo::class create loon::classes::SerialaxesGlyphVisual {

    superclass  ::loon::classes::GlyphVisual

    variable angles xdirs ydirs andrewsSeriesLength

    constructor {args} {
	set angles {}
	set xdirs {}
	set ydirs {}
	set andrewsSeriesLength 40

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


    method TrajectoryCoords {glyph s h andrews} {
	set coords {}

	set p [llength $glyph]
	set x [expr {-1.0*($p-1)/2.0*$s}]

	if {$andrews} {

	  set glyph [::loon::listfns::fourierTrans $glyph $andrewsSeriesLength]

	  set ming [::loon::listfns::min $glyph]

	  foreach g $glyph {
	    # shift `g` greater than zero
	    # try to avoid radial axes overlap
	    set newg [expr {$g - $ming}]

	    lappend coords $x [expr {(1-double($newg))*$h-$h/2.0}]
	    set x [expr {$x+$s}]
	  }

	} else {
	  foreach g $glyph {

	    lappend coords $x [expr {(1-double($g))*$h-$h/2.0}]
	    set x [expr {$x+$s}]
	  }
	}

	return $coords
    }

    ## This Function creates only the canvas items...
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

    method StarCoords {glyph radius andrews {inverse FALSE}} {
	set coords {}

    if {$andrews} {

	  set glyph [::loon::listfns::fourierTrans $glyph $andrewsSeriesLength]
	  set ming [::loon::listfns::min $glyph]

	  foreach g $glyph xdir $xdirs ydir $ydirs {

	    # shift `g` greater than zero
	    # try to avoid radial axes overlap
	    set newg [expr {$g - $ming}]

	    if {$inverse} {
		  lappend coords [expr {$radius * (1 - $newg) * $xdir}]
		  lappend coords [expr {$radius * (1 - $newg) * $ydir}]
	    } else {
		  lappend coords [expr {$radius * $newg * $xdir}]
		  lappend coords [expr {$radius * $newg * $ydir}]
	    }
	  }

	} else {
	  foreach g $glyph xdir $xdirs ydir $ydirs {
	    if {$inverse} {
		lappend coords [expr {$radius * (1 - $g) * $xdir}]
		lappend coords [expr {$radius * (1 - $g) * $ydir}]
	    } else {
		lappend coords [expr {$radius * $g * $xdir}]
		lappend coords [expr {$radius * $g * $ydir}]
	    }
	  }
	}
	return $coords
    }

    method DrawStar {p canvas ind} {
	my variable glyphObject

    if {[set ${glyphObject}::andrews]} {
	   set p $andrewsSeriesLength
	}

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

	set andrews [set ${glyphObject}::andrews]
	set p [llength [set ${glyphObject}::sequence]]

	if {[set ${glyphObject}::axesLayout] eq "radial"} {

	    set radius [::loon::map_star_size $size]

   	    if {$andrews} {

   	       if {[set ${glyphObject}::scaling] == "variable"} {
   	         set r [expr $radius/[expr $p - 1]]
   	       } else {
   	         set r [expr $radius/[expr $p - 2.5]]
   	       }

	       # set radius [expr $p * 10]
	    } else {
	       set r $radius
	    }

    	## Star
	    set coords [my StarCoords\
			    [lindex [set ${glyphObject}::glyphs] $ind]\
			    $r $andrews]

	    set nextid 0

	    if {[set ${glyphObject}::showEnclosing]} {
		  uplevel #0 [list $canvas coords [lindex $ids 0]\
			    	-$radius -$radius $radius $radius]
		  incr nextid
	    }

	    ## update glyph
	    if {[set ${glyphObject}::showArea]} {
		uplevel #0 [list $canvas coords [lindex $ids $nextid] $coords]
	    } else {
		## uses lines, hence need to close shape
		uplevel #0 [list $canvas coords [lindex $ids $nextid]\
				[concat $coords {*}[lrange $coords 0 1]]]
	    }
	    incr nextid

	    set showAxes [set ${glyphObject}::showAxes]

	    ## Axes
	    if {$andrews} {

	      if {$showAxes} {

	        set newIds {}

	        foreach index [::loon::listfns::lseq2 0 [expr [llength $ids] - 1] [expr $p + 1]] {
	             lappend newIds [lindex $ids [tcl::mathfunc::round $index]]
	        }

		    ## if enclosing is shown draw axes out to enclosing
		    ## otherwise only as far as the star goes
		    if {[set ${glyphObject}::showEnclosing]} {
		        foreach xdir $xdirs\
		        	ydir $ydirs\
			        id [lrange $ids $nextid end] {

			           if {$id in $newIds} {
			              uplevel #0 [list $canvas coords $id 0 0\
				         	    [expr {$radius * $xdir}]\
					            [expr {$radius * $ydir}]]
			           }
			        }
		    } else {
		      foreach {xstar ystar} [concat $coords {*}[lrange $coords 0 1]]\
		   	     id [lrange $ids $nextid end] {
		  	       if {$id in $newIds} {
		  	         	 uplevel #0 [list $canvas coords $id 0 0 $xstar $ystar]
		  	       }
		      }
		    }
	      }
	    } else {
	      if {$showAxes} {
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
	    }

	} else {

	    set s [::loon::map_pc_size $size]
	    set h [expr {4.0*$s}]

	    ## axes-distance
	    if {$andrews} {
	      set s [expr $s/4.0]
	      set h [expr $h/4.0]
	    }

	    ## Update Trajectory
	    set coords [my TrajectoryCoords\
			    [lindex [set ${glyphObject}::glyphs] $ind]\
			    $s $h $andrews]

	    if {$andrews} {

	      set w1 [::loon::listfns::min [::loon::listfns::getOdd $coords]]
	      set w2 [::loon::listfns::max [::loon::listfns::getOdd $coords]]

	      set h1 [::loon::listfns::min [::loon::listfns::getEven $coords]]
	      set h2 [::loon::listfns::max [::loon::listfns::getEven $coords]]

	      set s [expr $andrewsSeriesLength/$p * $s]

	    } else {
          set w1 [expr {-($p-1)/2.0*double($s)}]
          set w2 [expr {($p-1)/2.0*double($s)}]

          set h1 [expr {-$h/2.0}]
          set h2 [expr {$h/2.0}]
	    }

	    set nextid 0
	    if {[set ${glyphObject}::showEnclosing]} {
		uplevel #0 [list $canvas coords [lindex $ids 0] $w1 $h1 $w2 $h2]
		incr nextid
	    }

        set showAxes [set ${glyphObject}::showAxes]

	    if {$showAxes} {
		set x [expr {$w1+$s}]

		foreach id [lrange $ids $nextid end-1] {
		    uplevel #0 [list $canvas coords $id $x $h1 $x $h2]
		    set x [expr {$x+$s}]
		}
	    }

	    if {[set ${glyphObject}::showArea]} {
		   set coords [concat $w1 $h2 $coords $w2 $h2]
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
