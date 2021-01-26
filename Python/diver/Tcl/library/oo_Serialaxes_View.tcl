
oo::class create ::loon::classes::Serialaxes_View {

    superclass ::loon::classes::withCanvasAndItemBindings\
	::loon::classes::Layered_View

    ## Plot_View is more for zooming

    variable parallelVisual starsVisual modelVisual view model layerBinding\
	controller

    constructor {Path} {

	set parallelVisual ""
	set starsVisual ""

	set modelVisual ""

	set layerBinding ""

	set view [self]
	set model $Path

	next $Path

	my variable canvas
	$canvas configure -bg white

	set controller [::loon::classes::Serialaxes_Controller new [self]]

    }



    method setPlotModel {Model} {

	my variable plotModel canvas plotModelStateBinding clone_model

	if {$plotModel ne ""} {
	    error "currently serialaxes cant switch the model"
	}

	if {$layerBinding ne ""} {
	    catch {$plotModel systembind layer delete $layerBinding}
	    set layerBinding ""
	}

	## ItemLabel Controller
	$controller setModel $Model

	## Currently copied from Plot_View
	## as no zoom and pan is implemented
	if {$plotModelStateBinding ne ""} {
	    $plotModel systembind state delete $plotModelStateBinding
	    set plotModelStateBinding ""
	}

	set plotModel [info object namespace $Model]


	set starsVisual [::loon::classes::StarglyphsStackedVisual new\
			     $plotModel "model" $canvas map]

	set parallelVisual [::loon::classes::ParallelCoordinatesVisual new\
				$plotModel "model" $canvas map]

	if {[$Model cget -axesLayout] eq "radial"} {
	    my setModelVisual $starsVisual
	} else {
	    my setModelVisual $parallelVisual
	}

    set clone_model $Model

	if {$plotModel ne ""} {
	    set plotModelStateBinding [$Model systembind state add all "[self] plotUpdate %e"]
	}

	if {$Model ne ""} {
	    set layerBinding [$Model systembind layer add all [list [self] layerUpdate %l %e]]
	}

	my redraw
    }

    method setModelVisual {obj} {
	my delete
	set modelVisual $obj
	$controller setModelVisual $obj
	my redraw
    }

    method redraw {} {
	$modelVisual redraw
    }

    method recolor {} {
	$modelVisual recolor
    }

    method delete {} {
	if {$modelVisual ne ""} {
	    $modelVisual clear
	}
    }

    method plotUpdate {events} {
	#puts "modelupdate with events: $events"
	my variable plotModel

	if {$events eq "selected" || $events eq "color"} {
	    my recolor
	} elseif {$events eq "destroy"} {
	    ## Do nothing
	} else {
	    if {"axesLayout" in $events} {
		if {[${plotModel}::my cget -axesLayout] eq "radial"} {
		    my setModelVisual $starsVisual
		} else {
		    my setModelVisual $parallelVisual
		}
	    }
	    my redraw
	}
    }

    method AddLayer {layer} {

	my variable canvas map clone_model
	if {![$clone_model layer isGroup $layer]} {

	    set cmd [format "::loon::classes::%sVisual"\
			 [string toupper [$clone_model layer getType $layer] 0]]

	    dict set visuals $layer\
		[$cmd new [$clone_model layer getObject $layer] [self] $layer $canvas $map]
	}
    }

    method layerUpdate {layer event} {

	my variable canvas clone_model plotModel

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
		foreach l [$clone_model layer getDescendants $layer TRUE] {
		    if {[dict exists $visuals $l]} {
			[dict get $visuals $l] setVisibility FALSE
		    }
		}
	    }
	    show {
		foreach l [$clone_model layer getDescendants $layer TRUE] {
		    if {[dict exists $visuals $l]} {
			[dict get $visuals $l] setVisibility TRUE
		    }
		}
	    }
	}

    }

    method MoveLayer {layer} {
	my variable clone_model canvas

	set descendants [$clone_model layer getDescendants root FALSE]
	#puts "$layer, descendandts: $descendants"
	foreach l $descendants {

	    if {![$clone_model layer isGroup $l]} {
		uplevel #0 [list $canvas lower $l]
	    }

	}

    }

    method DeleteLayer {layer} {
	if {[dict exists $visuals $layer]} {
	    [dict get $visuals $layer] destroy
	    dict unset visuals $layer
	}
    }


    method canvasResize {w h} {
	next $w $h
	my redraw
    }

    method updateViewport {} {}
}
