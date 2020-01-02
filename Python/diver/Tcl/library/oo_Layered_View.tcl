

::oo::class create ::loon::classes::Layered_View {
    
    superclass ::loon::classes::Plot_View


    variable visuals layerBinding
    
    constructor {Path} {
	
	set visuals [dict create]
	
	set layerBinding ""

	next $Path
	
    }

    ## visuals need to destroy themself
    destructor {
	next
	#dict for {visual object} $visuals {$object destroy}
    }
    
    method InfoDebug {args} {
	next visuals layerBinding {*}$args
    }
    
    method setPlotModel {Model} {
	my variable plotModel

	if {$layerBinding ne ""} {
	    catch {$plotModel systembind layer delete $layerBinding}
	    set layerBinding ""
	}
	
	catch {dict for {visual object} $visuals {my DeleteLayer $visual}}
	set visuals [dict create]
	
	next $Model
	
	if {$plotModel ne ""} {
	    set layerBinding [$plotModel systembind layer add all [list [self] layerUpdate %l %e]]
	    my Init
	}
	
    }

    method Init {} {
	my variable plotModel
	
	if {$plotModel ne ""} {
	    set layers [$plotModel layer getDescendants "root"]
	    foreach layer $layers {
		my AddLayer $layer
	    }
	}

    }


    method getModelLayer {} {
	if {[dict exists $visuals model]} {
	    return [dict get $visuals model]
	} else {
	    return ""
	}
    }

    method AddLayer {layer} {
	my variable canvas map plotModel
	if {![$plotModel layer isGroup $layer]} {
	    
	    set cmd [format "::loon::classes::%sVisual"\
			 [string toupper [$plotModel layer getType $layer] 0]]
	    
	    dict set visuals $layer\
		[$cmd new [$plotModel layer getObject $layer] [self] $layer $canvas $map]
	}
    }
    
    method layerUpdate {layer event} {
	my variable canvas plotModel
	
	# currently only one event for one layer at a time is implemented
	switch -- $event {
	    add {
		my AddLayer $layer
	    }
	    delete {
		my DeleteLayer $layer
	    }
	    move {
		my MoveLayer $layer
	    }
	    hide {
		foreach l [$plotModel layer getDescendants $layer TRUE] {
		    if {[dict exists $visuals $l]} {
			[dict get $visuals $l] setVisibility FALSE
		    }
		}		
	    }
	    show {
		foreach l [$plotModel layer getDescendants $layer TRUE] {
		    if {[dict exists $visuals $l]} { 
			[dict get $visuals $l] setVisibility TRUE
		    }
		}
	    }
	}
	
    }
    
    method MoveLayer {layer} {
	my variable plotModel canvas
	
	set descendants [$plotModel layer getDescendants root FALSE]
	#puts "$layer, descendandts: $descendants"
	foreach l $descendants {
	    
	    if {![$plotModel layer isGroup $l]} {
		uplevel #0 [list $canvas lower $l]
	    }
	    
	}	

	## This method does not work because we do not add group information
	## onto the canvas
#	set ancestors [concat $layer [$plotModel layer getAncestors $layer]]
	
#	foreach a $ancestors {
#	    if {$a eq "root"} {
#		break
#	    }
#	    set siblings [$plotModel layer getSiblings $a]
#	    if {[lindex $siblings 0] ne ""} {
#		uplevel #0 [list $canvas lower $layer [lindex $siblings 0]]
#		break
#	    } elseif {[lindex $siblings 1] ne ""} {
#		uplevel #0 [list $canvas raise $layer [lindex $siblings 1]]
#		break
#	    } 
#	}
	
    }
    
    method DeleteLayer {layer} {
	if {[dict exists $visuals $layer]} {
	    [dict get $visuals $layer] destroy
	    dict unset visuals $layer
	}	
    }

    method redraw {} {
	dict for {visual object} $visuals {$object redraw}
	next
    }

    
    method updatePan {dPanX dPanY} {
	my variable canvas map
       	
	## move all layers
	uplevel #0 [list $canvas move layer {*}[$map dPan2dS $dPanX $dPanY]]
	
	dict for {visual object} $visuals {$object updatedPan $dPanX $dPanY}
	
    }
    
    method updateZoomPan {oldPanX oldPanY oldZoomX oldZoomY} {
	dict for {visual object} $visuals {$object updateZoomPan\
					       $oldPanX $oldPanY $oldZoomX $oldZoomY}
    }
    
    method updateCoords {} {
	dict for {visual object} $visuals {$object updateCoords}
    }
    

    method reorder {} {
	
    }
    
    method getModelVisual {} {
	return [dict get $visuals model]
    }
}
