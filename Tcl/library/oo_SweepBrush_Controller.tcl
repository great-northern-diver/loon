
::oo::class create ::loon::classes::SweepBrush_Controller {
    
    superclass ::loon::classes::ButtonPressMotion_Controller
    
    variable sb_soi sweep brush modelLayer inSweepingMode n_var selected_var\
	selectionLogic_var selectBy_var resizeBinding bp_modelStateBinding viewMap\
	inBrushResizeMode inBrushSelectionUpdate
    
    constructor {view} {
	
	array set sweep {init FALSE n 0}
	array set brush {}
	
	## states of interest
	set sb_soi {n selected selectionLogic selectBy}
	
	set inSweepingMode TRUE
	set inBrushResizeMode FALSE
	set inBrushSelectionUpdate FALSE
	set modelLayer ""
	set bp_modelStateBinding ""
	
	set viewMap [set [uplevel #0 [list [info object namespace $view]::my varname map]]]
	
	next $view
	
	
    }
    
    method init {} {
	my variable view canvas
	
	next
	
	set resizeBinding [bind $canvas <Configure> "+[self] resizeCanvas %x %y"]
	
    }

    
    method setModel {Model} {
	
	if {$bp_modelStateBinding ne ""} {
	    $model systembind state delete $bp_modelStateBinding
	    set bp_modelStateBinding ""
	}
	
	if {$Model ne ""} {
	    set bp_modelStateBinding [$Model systembind state add all\
					  "[self] plotUpdateSweepBrush %e"]
	}


	set ns [info object namespace $Model] 
	foreach state $sb_soi {
	    set ${state}_var [uplevel #0 [list ${ns}::my varname $state]]
	}
	
	set modelLayer ""

	next $Model
    }    

    method button1_press {x y} {
	my variable canvas
	my save_coords $x $y
	if {$inSweepingMode} {
	    my sweep_start $x $y
	} else {
	    if {[lindex [$canvas gettags current] 1] eq "loon_brush_handle"} {
		set inBrushResizeMode TRUE
	    } else {
		set inBrushResizeMode FALSE
		my brush_jump $x $y
	    }
	    
	}
    }
    
    method button1_motion {x y} {	
	if {$inSweepingMode} {
	    my sweep_move $x $y
	} else {
	    if {$inBrushResizeMode} {
		my brush_resize $x $y
	    } else {
		my brush_move $x $y
	    }
	}
    }
    
    method button1_release {x y} {
	if {$inSweepingMode} {
	    my sweep_end $x $y
	}
    }


    ## Sweep
    ## is end effectively select/reset or toggle a point
    method sweep_start {x y {multipleSelect FALSE} {invertSelectedPoint TRUE}} {
	my variable  model view
	
	set sweep(init) FALSE
	set sweep(x0) $x
	set sweep(y0) $y
	set sweep(n) [set $n_var]
		
	if {$modelLayer eq "" } {
	    set modelLayer [$view getModelLayer]
	}

	if {$sweep(n) eq 0} {return}

	# Select individual point if clicked on one
	set which [$modelLayer indexWithCurrentTag]
	
	# Select Single Point: reset selection		
	# or single bin	
	if {[llength $which] >= 1} {

	    if {$multipleSelect} {
		set selected [set $selected_var]
	    } else {
		set selected [lrepeat $sweep(n) 0]
	    }
	    
	    foreach whi $which {
		if {$invertSelectedPoint} {
		    if {[lindex $selected $whi]} {
			lset selected $whi 0
		    } else {
			lset selected $whi 1		
		    }
		} else {
		    lset selected $whi 1
		}
	    }
	    
	} elseif {$multipleSelect} {
	    return
	} else {
	    set selected FALSE
	}
	
	# Configure should catch if not changed
	$model configure -selected $selected
	uplevel #0 [list update idletasks]
    }

    method sweep_move {x y} {
	my variable model canvas
	
	if {[catch {set coords [list $sweep(x0) $sweep(y0) $x $y]}]} {
	    set coords [list $x $y $x $y]
	}
	
	
	if {$sweep(init)} {
	    $canvas coords $sweep(id) {*}$coords	    
	} else {
	    # create new sweep rectangle
	    set sweep(id) [$canvas create rect {*}$coords\
			       -fill ""\
			       -outline $::loon::Options(brush_color)\
			       -width 2\
			       -tag "sweep-rect"]
	    
	    # cache previous selection state
	    set sweep(selected_cached) [set $selected_var]	   
	    set sweep(init) TRUE
	}
	
	## Temporary Select Below Sweep Rectangle
	if {$sweep(n) > 0} {
	    set selected $sweep(selected_cached)
	    
	    set which [$modelLayer indecesWithinRect {*}$coords]
	    
	    if {[llength $which] > 0} {
		set logic [set $selectionLogic_var]
		
		set selState [::loon::listfns::subset $selected $which]
		set newSelState [::loon::applySelectionLogic $selState $logic]
		
		foreach w $which sel $newSelState {
		    lset selected $w $sel
		}
	    }
	    $model configure -selected $selected
	}	    
	
	uplevel #0 [list update idletasks]
    }
    
    method sweep_end {x y} {
	my variable canvas
	
	$canvas delete "sweep-rect"
	set sweep(init) FALSE
	uplevel #0 [list update idletasks]
    }

    

    ## Brush
    method plotUpdateSweepBrush {events} {
	if {"selectBy" in $events} {
	    if {[set $selectBy_var] eq "sweeping"} {
		set inSweepingMode TRUE
		my brush_remove
	    } else {
		set inSweepingMode FALSE
		my brush_init	    
	    }
	}
	if {!$inSweepingMode} {
	    if {"selected" in $events && !$inBrushSelectionUpdate} {
		set brush(toggled_cached) {}
		set brush(selected_cached) [set $selected_var]
		my SelectBelowBrush
	    } elseif {"zoomX" in $events || "zoomY" in $events ||\
		    "panX" in $events || "panY" in $events ||\
		    "selectionLogic" in $events} {
		my SelectBelowBrush
	    }
	    
	}
    }
    
    method resizeCanvas {x y} {
	if {!$inSweepingMode} {
	    my SelectBelowBrush
	}
    }
    

    method brush_init {} {

	my variable canvas model
	
	if {$model eq ""} {return}
	
	# State of points below brush
	set brush(selected_cached) [set $selected_var]
	
	
	## place brush in the upper right corner
	set vpx0 [$viewMap getVpx0]
	set vpy0 [$viewMap getVpy0]
	
	set x0 [expr {$vpx0 + 10}]
	set y0 [expr {$vpy0 + 10}]	    	   
	set x1 [expr {$x0 + 50}]
	set y1 [expr {$y0 + 50}]
	
	set brush(id)\
	    [$canvas create rect $x0 $y0 $x1 $y1\
		 -outline $::loon::Options(brush_color)\
		 -width 2 -fill "" -tag [list "loon_brush" "loon_brush_rect"]]
	
	set brush(handle_id)\
	    [$canvas create rect 0 0 6 6\
		 -fill $::loon::Options(brush_color_handle)\
		 -tag [list "loon_brush" "loon_brush_handle"]]
	
	$canvas move $brush(handle_id)\
	    [expr {$x1 - 3}]\
	    [expr {$y1 - 3}]
	
	set brush(x0) $x0; set brush(y0) $y0
	set brush(x1) $x1; set brush(y1) $y1
	
	my SelectBelowBrush
    }
    
    
    method brush_jump {args} {
	my brush_move {*}$args
    }
        
    ## Jump or Move
    method brush_move {x y {permanent FALSE}} {	
	my variable canvas
	
	set dx [expr {$x - $brush(x1)}]
	set dy [expr {$y - $brush(y1)}]
	
	$canvas move "loon_brush" $dx $dy

	set coords [$canvas coords "loon_brush_rect"]
	set brush(x0) [lindex $coords 0]
	set brush(y0) [lindex $coords 1]
	set brush(x1) [lindex $coords 2]
	set brush(y1) [lindex $coords 3]

	my SelectBelowBrush $permanent
    }

    method brush_resize {x y {permanent FALSE}} {
	my variable canvas
	
	set x0 $brush(x0)
	set y0 $brush(y0)
	
	set x1 [expr {max($x0, $x)}]
	set y1 [expr {max($y0, $y)}]
	
	$canvas coords "loon_brush_rect" $x0 $y0 $x1 $y1 
	$canvas coords "loon_brush_handle"\
	    [expr {$x1-3}] [expr {$y1-3}]\
	    [expr {$x1+3}] [expr {$y1+3}]
	
	set brush(x1) $x1
	set brush(y1) $y1
	
	my SelectBelowBrush $permanent
    }
    
    method brush_remove {} {
	
	my variable canvas model
	
	$canvas delete $brush(id)
	$canvas delete $brush(handle_id)

	set brush(id) {}
	set brush(handle_id) {}
	set brush(toggled_cached) {}

	uplevel #0 [list $model configure -selected $brush(selected_cached)]
	set brush(selected_cached) {}
	
    }

    method SelectBelowBrush {{permanent FALSE}} {
	my variable canvas model view

	if {[set $n_var] eq "0"} {return}
	
	set brush_coords [$canvas coords $brush(id)]
	if {[llength $brush_coords] ne 4} {
	    puts "warning: brush rectangle not found"
	    return
	}
	if {$modelLayer eq "" } {
	    set modelLayer [$view getModelLayer]
	}

	set whichNew [$modelLayer indecesWithinRect {*}$brush_coords]
	set nNew [llength $whichNew]


	set selected $brush(selected_cached)
	set which [$modelLayer indecesWithinRect {*}$brush_coords]
	
	if {[llength $which] > 0} {
	    set logic [set $selectionLogic_var]
	    
	    set selState [::loon::listfns::subset $selected $which]
	    set newSelState [::loon::applySelectionLogic $selState $logic]
	    
	    if {$logic eq "invert" && $permanent} {
		## Need to check that selected states do not toggle
		## below brush when brush moves
		
		## Omit the points that have already been toggled
		
		if {[llength $brush(toggled_cached)] eq 0} {
		    ## invert all
		    foreach w $which sel $newSelState {
			lset selected $w $sel
		    }
		} else {
		    foreach w $which sel $newSelState {
			if {$w ni $brush(toggled_cached)} {
			    lset selected $w $sel
			}
		    }
		}

		set brush(toggled_cached) $which
		
	    } else {
		set brush(toggled_cached) {}
		
		foreach w $which sel $newSelState {
		    lset selected $w $sel
		}
	    }
	    
	    if {$permanent} {
		set brush(selected_cached) $selected
	    }
	}

	set inBrushSelectionUpdate TRUE
	$model configure -selected $selected
	uplevel #0 [list update idletasks]
	set inBrushSelectionUpdate FALSE
	
    }
    

    # Meta Keys
    method meta_b1 {x y meta} {
	my save_coords $x $y
	
	if {$meta eq $::loon::Options(metaMulSel)} {
	    if {$inSweepingMode} {
		my sweep_start $x $y TRUE
	    } else {
		my brush_jump $x $y TRUE
	    }
	    
	} elseif {$meta eq $::loon::Options(metaTempMove)} {
	    if {$inSweepingMode} {
		my sweep_start $x $y FALSE
	    }
	    
	} else {
	    next $x $y $meta
	}
	
    }

    method meta_b1_motion {x y meta} {
	
	if {$meta eq $::loon::Options(metaMulSel)} {

	    if {$inSweepingMode} {
		my sweep_move $x $y
	    } else {
		my brush_move $x $y TRUE
	    }
	    
	} else {
	    next $x $y $meta
	}
	
    }

    method meta_meta_b1 {x y meta1 meta2} {
	my save_coords $x $y
	#puts "$meta1 $meta2 Button-1"
	## if temporary move all selected points
	if {$meta1 eq $::loon::Options(metaMulSel) &&\
		$meta2 eq $::loon::Options(metaTempMove) ||\
		$meta2 eq $::loon::Options(metaMulSel) &&\
		$meta1 eq $::loon::Options(metaTempMove)} {    
    	    
	    if {$inSweepingMode} {
		my sweep_start $x $y TRUE FALSE
	    }
	} else {
	    next $x $y $meta1 $meta2
	}
    }

    
   
}
