
oo::class create ::loon::classes::Serialaxes_View {

    superclass ::loon::classes::withCanvasAndItemBindings\
	::loon::classes::Plot_View
    
    ## Plot_View is more for zooming

    variable parallelVisual starsVisual modelVisual view model\
	controller
    
    constructor {Path} {
	
	set parallelVisual ""
	set starsVisual ""

	set modelVisual ""

	set view [self]
	set model $Path

	next $Path

	my variable canvas
	$canvas configure -bg white


	set controller [::loon::classes::Serialaxes_Controller new [self]]
	
    }
    
    
    
    method setPlotModel {Model} {
	my variable plotModel canvas plotModelStateBinding plotModel
	
	if {$plotModel ne ""} {
	    error "currently serialaxes cant switch the model"
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

	
	if {$plotModel ne ""} {
	    set plotModelStateBinding [$Model systembind state add all "[self] plotUpdate %e"]
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
    
    method canvasResize {w h} { 
	next $w $h
	my redraw
    }
    
    method updateViewport {} {}
    


}
