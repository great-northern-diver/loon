::oo::class create ::loon::classes::XYPair_Model {

    superclass ::loon::classes::Plot_Model\
	::loon::classes::withGlyphs\
	::loon::classes::withItemLabels

    variable minX minY maxX maxY

    constructor {} {
	
	set minX ""
	set minY ""
	set maxX ""
	set maxY ""
	
	next
	
	## Coordinates
	my New_state x double n ""
	my New_state y double n ""
	my New_state xTemp tempcoords n ""    ;# double  n||0
	my New_state yTemp tempcoords n ""
	
	my New_state color color n [lindex  $::loon::Options(colors) 0]; #"#377EB8"
	my New_state selected boolean n FALSE
	my New_state active boolean n TRUE
	my New_state size double n 4

	my New_state tag string n ""
	
	my SetInitStates n {x y} ;# set lead states

	my AddNotLinkableStates {x y xTemp yTemp} 
	my setLinkedStates [list color selected active size]

	my New_state useLoonInspector boolean 1 TRUE
	
	my New_state selectBy factor 1 sweeping {sweeping brushing} 
	my New_state selectionLogic factor 1 select {select deselect invert} 

	## For documentation, i.e. info states 
	my SetStateDescription x "x coordinates"
	my SetStateDescription y "y coordinates"
	my SetStateDescription xTemp\
	    "if specified, these are x coordinates used instead those from the -x state"
	my SetStateDescription yTemp\
	    "if specified, these are y coordinates used instead those from the -y state"

	my SetStateDescription color "colors of the point glyphs"
	my SetStateDescription selected\
	    "selected points are highlighted and can be modified with the inspector"
	my SetStateDescription active\
	    "active points get rendered, inactive ones do not"
	my SetStateDescription size "sizes of point glyphs"

	my SetStateDescription tag\
	    "point glyphs have tags associated that can be used for item bindings"
	
	my SetStateDescription useLoonInspector\
	    "boolean to specify whether to report interaction events to the loon inspector or not"
	my SetStateDescription selectBy\
	    "sweeping or brushing: interactive selection method"
	my SetStateDescription selectionLogic\
	    "select, deselect or invert: logic for selection"
	
	
	
    }

    method GetDefaultValue {state length} {
	if {$state eq "tag"} {
	    if {$length eq 0} {return ""}
	    return [::loon::listfns::ljoin\
			[lrepeat $length "point"]\
			[::loon::listfns::lseq 0 [expr {$length-1}]]]
	} elseif {$state eq "xTemp" || $state eq "yTemp"} {
	    return ""
	} else {
	    return [next $state $length]
	}
    } 
    

    method EvalConfigure {} {
	my variable confDict


	next

	## Delete xTemp, yTemp if an x or y is specified, respectively
	set hasX  [dict exists $confDict new_x]
	set hasY  [dict exists $confDict new_y]
	set hasXTemp  [dict exists $confDict new_xTemp]
	set hasYTemp  [dict exists $confDict new_yTemp]
	
	if {[dict exists $confDict new_rotate3DX] || $hasX && !$hasXTemp} {
	    dict set confDict new_xTemp ""
	}
	if {[dict exists $confDict new_rotate3DY] || $hasY && !$hasYTemp} {
	    dict set confDict new_yTemp ""
	}
	
	## Now apply state rules
	## only active points can be selected	
	set hasActive [dict exists $confDict new_active]
	set hasSelected [dict exists $confDict new_selected] 
	if { $hasActive || $hasSelected } {
	    
	    if {$hasActive} {
		set act [dict get $confDict new_active]
	    } else {
		my variable active
		set act $active
	    } 
	   
	    if {$hasSelected} {
		set sel [dict get $confDict new_selected]
	    } else {
		my variable selected
		set sel $selected
	    }

	    set needChange FALSE
	    foreach a $act s $sel {
		if {$s && !$a} {
		    set needChange TRUE
		    break
		}
	    }
	    
	    set newSelected $sel
	    set i 0
	    foreach a $act s $sel {
		if {$s && !$a} {
		    lset newSelected $i FALSE
		}
		incr i
	    }
	    dict set confDict new_selected $newSelected	    
	}
    }
    
    method HookAfterStatesSet {} {
	my variable changedStates n x y xTemp yTemp
	
	next
	
	if {$n eq 0} {
	    ## default values
	    set minX ""
	    set maxX ""
	    set minY ""
	    set maxY ""
	} else {	    
	    if {"x" in $changedStates || "xTemp" in $changedStates} {
		set mm [::loon::listfns::MinMax $x]
		set minX [lindex $mm 0]
		set maxX [lindex $mm 1]
		
		if {$xTemp ne {}} {
		    set mmt [::loon::listfns::MinMax $xTemp]
		    if {[lindex $mmt 0] < $minX} {
			set minX [lindex $mmt 0]
		    }
		    if {[lindex $mmt 1] > $maxX} {
			set maxX [lindex $mmt 1]
		    }
		}
		
	    }
	    if {"y" in $changedStates || "yTemp" in $changedStates} {
		set mm [::loon::listfns::MinMax $y]
		set minY [lindex $mm 0]
		set maxY [lindex $mm 1]
		
		if {$yTemp ne {}} {
		    set mmt [::loon::listfns::MinMax $yTemp]
		    if {[lindex $mmt 0] < $minY} {
			set minY [lindex $mmt 0]
		    }
		    if {[lindex $mmt 1] > $maxY} {
			set maxY [lindex $mmt 1]
		    }
		}
		
	    }
	    
	}
    }


    method move {how {which "selected"} args} {
	my variable swapAxes x y xTemp yTemp zoomX zoomY deltaX deltaY n
	
	
	## indices
	set sel [my ProcessWhich $which n]

	set n_sel [llength $sel]
	if {$n_sel eq 0} {return}
	
	set swap $swapAxes
	
	switch -- $how {
	    reset {
		
		if {$n_sel eq $n} {
		    ## reset all
		    set tmp_x {}
		    set tmp_y {}
		} else {
		    if {[llength $xTemp] > 0} {
			set tmp_x $xTemp
		    } else {
			set tmp_x $x
		    }
		    
		    if {[llength $yTemp] > 0} {
			set tmp_y $yTemp
		    } else {
			set tmp_y $y
		    }

		    foreach ind $sel {
			lset tmp_x $ind [lindex $x $ind]
			lset tmp_y $ind [lindex $y $ind]
		    }
		    
		    ## Check if all are reset now
		    set isXreset TRUE
		    foreach x_orig $x x_new $tmp_x {
			if {$x_new ne $x_orig} {
			    set isXreset FALSE
			    break
			}
		    }
		    if {$isXreset} {
			set tmp_x {}
		    }
		    set isYreset TRUE
		    foreach y_orig $y y_new $tmp_y {
			if {$y_new ne $y_orig} {
			    set isYreset FALSE
			    break
			}
		    }
		    if {$isYreset} {
			set tmp_y {}
		    }
		}
		
		my configure -xTemp $tmp_x -yTemp $tmp_y
	    }
	    valign - 
	    halign {
		
		if {$how eq "halign" && !$swap\
			|| $how eq "valign" && $swap} {
		    set dir y
		} else {
		    set dir x
		}
		
		if {$dir eq "x"} {
		    if {[llength $xTemp] eq 0} {
			set data $x
		    } else {
			set data $xTemp
		    }
		} else {
		    if {[llength $yTemp] eq 0} {
			set data $y
		    } else {
			set data $yTemp
		    }
		}
		
		set coords [::loon::listfns::subset\
				$data $sel]
		
		set mm [::loon::listfns::MinMax $coords]
		set middle [expr {([lindex $mm 0] + [lindex $mm 1])/2}]
		
		foreach s $sel {
		    lset data $s $middle
		}
		
		my configure -${dir}Temp $data		
	    }
	    vdist -
	    hdist {
		
		if {$how eq "hdist" && !$swap\
			|| $how eq "vdist" && $swap} {
		    set dir x
		} else {
		    set dir y
		}
		
		
		if {$dir eq "x"} {
		    if {[llength $xTemp] eq 0} {
			set data $x
		    } else {
			set data $xTemp
		    }
		} else {
		    if {[llength $yTemp] eq 0} {
			set data $y
		    } else {
			set data $yTemp
		    }
		}
		
		set coords [::loon::listfns::subset $data $sel]
		
		set mm [::loon::listfns::MinMax $coords]
		set min [lindex $mm 0]
		set max [lindex $mm 1]
		
		
		## center it on the screen
		if {[expr {$max -$min} < 0.0001]} {
		    if {$how eq "hdist" && !$swap || $how eq "vdist" && $swap} {
			set zoom $zoomX
			set range $deltaX
		    } else {
			## need height
			set zoom $zoomY
			set range $deltaY
		    }
		    set h [expr {$range/$zoom/5.0}]
		    set min [expr {$min - $h}]
		    set max [expr {$max + $h}]
		}
		
		set nsel [llength $coords]
		set step [expr {double($max - $min)/($nsel-1)}]
		
		## This seems to be buggy
		## in fact, it does not give the correct order for values <0
		set indices [lsort -real -increasing -indices $coords]
		
		set newcoords [lrepeat $nsel $min]

		set tval $min
		foreach i $indices {
		    lset newcoords $i $tval
		    set tval [expr {$tval+$step}]
		}
		
		foreach s $sel c $newcoords {
		    lset data $s $c
		}
		
		my configure -${dir}Temp $data
		
	    }
	    grid {
		if {[llength $xTemp] eq 0} {
		    set xvals $x
		}  else {
		    set xvals $xTemp
		}
		if {[llength $yTemp] eq 0} {
		    set yvals $y
		}  else {
		    set yvals $yTemp
		}
		
		set xcoords [::loon::listfns::subset $xvals $sel]
		set ycoords [::loon::listfns::subset $yvals $sel]


		
		if {$n_sel eq 1} {
		    return
		}
		
		set xmm [::loon::listfns::MinMax $xcoords]
		set ymm [::loon::listfns::MinMax $ycoords]
		
		set xmin [lindex $xmm 0]; set xmax [lindex $xmm 1]
		set ymin [lindex $ymm 0]; set ymax [lindex $ymm 1]
		
		set swap $swapAxes
		
		if {[expr {$xmax -$xmin} < 0.0001]} {
		    set h [expr {$deltaX/$zoomX/5.0}]
		    set xmin [expr {$xmin - $h}]
		    set xmax [expr {$xmax + $h}]
		}
		
		if {[expr {$ymax -$ymin} < 0.0001]} {
		    set h [expr {$deltaY/$zoomY/5.0}]
		    set ymin [expr {$ymin - $h}]
		    set ymax [expr {$ymax + $h}]
		}
		
		
		set nsel [llength $xcoords]
		
		if {$swap} {
		    set ny [expr {int(ceil(sqrt($nsel)))}]
		    set nx [expr {int(ceil(double($nsel)/$ny))}]

		    if {$nx > 1} {
			set xstep [expr {($xmax-$xmin)/double($nx-1)}]
		    } else {
			set xstep 0
			set xmin [expr {[expr {([lindex $xmm 0] + [lindex $xmm 1])/2.0}]}]
		    }
		    if {$ny > 1} {
			set ystep [expr {($ymax-$ymin)/double($ny-1)}]
		    } else {
			set ystep 0
			set ymin [expr {[expr {([lindex $ymm 0] + [lindex $ymm 1])/2.0}]}]
		    }
		    
		    set rankX [lsort -real -indices $xcoords]
		    
		    for {set i 0} {$i < $nx} {incr i} {
			set ind [lrange $rankX [expr {$i*$ny}]\
				     [expr {($i+1)*$ny -1}]]
			set rankY [lsort -real -indices [::loon::listfns::subset $ycoords $ind]]
			set ind2 [::loon::listfns::subset $ind $rankY]
			set j 0
			foreach w [::loon::listfns::subset $sel $ind2] {
			    lset xvals $w [expr {$xmin + $i*$xstep}]
			    lset yvals $w [expr {$ymin + $j*$ystep}]
			    incr j			
			}
		    }
		} else {
		    set nx [expr {int(ceil(sqrt($nsel)))}]
		    set ny [expr {int(ceil(double($nsel)/$nx))}]

		    if {$nx > 1} {
			set xstep [expr {($xmax-$xmin)/double($nx-1)}]
		    } else {
			set xstep 0
			set xmin [expr {[expr {([lindex $xmm 0] + [lindex $xmm 1])/2.0}]}]
		    }
		    if {$ny > 1} {
			set ystep [expr {($ymax-$ymin)/double($ny-1)}]
		    } else {
			set ystep 0
			set ymin [expr {[expr {([lindex $ymm 0] + [lindex $ymm 1])/2.0}]}]
		    }
		    
		    set rankY [lsort -real -indices $ycoords]
		    
		    for {set i 0} {$i < $ny} {incr i} {
			set ind [lrange $rankY [expr {$i*$nx}]\
				     [expr {($i+1)*$nx -1}]]
			set rankX [lsort -real -indices [::loon::listfns::subset $xcoords $ind]]
			set ind2 [::loon::listfns::subset $ind $rankX]
			set j 0
			foreach w [::loon::listfns::subset $sel $ind2] {
			    lset xvals $w [expr {$xmin + $j*$xstep}]
			    lset yvals $w [expr {$ymin + $i*$ystep}]
			    incr j			
			}
		    }
		}
		
		my configure -xTemp $xvals -yTemp $yvals
	    }
	    jitter {
		if {[llength $xTemp] eq 0} {
		    set t_x $x
		}  else {
		    set t_x $xTemp
		}
		if {[llength $yTemp] eq 0} {
		    set t_y $y
		}  else {
		    set t_y $yTemp
		}
		
		set s_x [::loon::listfns::subset $t_x $sel]
		set s_y [::loon::listfns::subset $t_y $sel]
		
		#puts -nonewline "-x jitter: "
		set j_x [::loon::listfns::jitter $s_x {*}$args]
		#puts -nonewline "-y jitter: "
		set j_y [::loon::listfns::jitter $s_y {*}$args]
		#puts ""
		
		foreach i $sel xi $j_x yi $j_y {
		    lset t_x $i $xi
		    lset t_y $i $yi
		}
		
		my configure -xTemp $t_x -yTemp $t_y
	    }	    
	    default {
		error "move \"$how\" is not valid. Use: valign,\
                             halign, vdist, hdist, grid, jitter, and reset."
	    }
	}
    }
}
