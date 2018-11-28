
oo::class create loon::classes::Worldview_View {

    superclass ::loon::classes::Layered_View
 
    variable controller bboxStateBindings resizeBinding canvas_pratio modelMap\
	worldBox viewareaOutline viewareaFill modeldataFill inModelInit\
	oldZoomRatio
    
    constructor {args} {
	my variable canvas map

	## add a binding for every layer to see if x and y are changing
	set bboxStateBindings [dict create]
	set resizeBinding ""
	set canvas_pratio 1
	set modelMap ""
	set inModelInit FALSE
	set oldZoomRatio -1
	
	next {*}$args

	## Change Canvas background color
	uplevel #0 [list $canvas configure -bg gray80]
	
	##    BoxVisual: Color Linecolor Linewidth Dash ...
	
	set worldBox [::loon::classes::BoxVisual new\
			  "" gray70 1 "" "LoonWorldBox" $canvas $map]
	
	set modeldataFill [::loon::classes::BoxVisual new\
			       gray95 "" 1 "" "LoonModelDataFill" $canvas $map]

	
	set viewareaFill [::loon::classes::BoxVisual new\
			      white "" 1 "" "LoonViewareaFill" $canvas $map]


	set viewareaOutline [::loon::classes::BoxVisual new\
				 "" black 2 "" "LoonViewareaOutline" $canvas $map]
	
	
	set controller [::loon::classes::Worldview_Controller new [self]]
	

    } 


    destructor {
	next
    }

    method ModelDestroyed {} {
	error "The code should not hit this part."
    }



    ## Make smaller worldview
    method Make {} {
	my variable canvas_width canvas_height
	set canvas_width 100
	set canvas_height 160
	next	
    }

    method updateCoords {} {
	$worldBox updateCoords
	$modeldataFill updateCoords
	$viewareaOutline updateCoords
	$viewareaFill updateCoords
	next
    }
    
    method redraw {} {
	$worldBox updateCoords
	$modeldataFill redraw
	$viewareaOutline redraw
	$viewareaFill redraw	
	next	
    }
    
    ## zoom and pan have no influence on map
    method plotUpdate {events} {
	my variable map swap_var

	if {"swapAxes" in $events} {
	    $map setSwap [set $swap_var]
	    my updateRange
	} elseif {"deltaX" in $events || "deltaY" in $events ||\
		      "showScales" in $events || "showLabels" in $events} {
	    my updateRange
	} elseif {"zoomX" in $events || "panX" in $events ||\
		      "zoomY" in $events || "panY" in $events} {
	    my variable zoomX_var zoomY_var
	    set zoomX [set $zoomX_var]
	    set zoomY [set $zoomY_var]
	    set r [expr {double($zoomX)/$zoomY}]
	    ## has the zoom ratio changed?
	    if {[expr {abs($r-$oldZoomRatio)}] > 0.001} {
		set oldZoomRatio $r
		my updateRange
	    }
	    my updateViewarea	    
	} elseif {"origin" in $events || "binwidth" in $events} {
	    ## not quite sure why the after idle is necessary
	    after idle [list [self] updateRange]
	} elseif {"rotate3DX" in $events || "rotate3DY" in $events } {
        my updateViewport
    }
    }
    
    method updateViewarea {} {
	my variable plotModel deltaX_var deltaY_var\
	    zoomX_var zoomY_var panX_var panY_var

	if {$plotModel ne ""} {
	    set deltaX [set $deltaX_var]
	    set deltaY [set $deltaY_var]
	    set zoomX [set $zoomX_var]
	    set zoomY [set $zoomY_var]
	    set panX [set $panX_var]
	    set panY [set $panY_var]
	    
	    
	    set toX [expr {$panX + $deltaX/double($zoomX)}]
	    set toY [expr {$panY + $deltaY/double($zoomY)}]
	    
	    $viewareaFill setCoords $panX $panY $toX $toY
	    $viewareaOutline setCoords $panX $panY $toX $toY		
	}
    }
    
    ## Use
    ## Map with zoom 1
    ## - Viewport is such that it has the same aspect ratio like the activewidget.
    ##    - The deltaX and deltaY need to be kept in sync with all layers
    ##    - Outline for all layered elemets
    ## - Use ...WorldviewVisuals if implemented
    ##    - Scatterplot Outline data points
    ## - Zoom Pan update rectangle that shows the visible area of the activewidget.

 
    method SetMap {} {
	my variable map mapns
	set map [::loon::classes::WorldviewMap new 1 1]

	set mapns [info object namespace $map]
    }

    method setPlotModel {Model} {
	my variable plotModel
	## be careful, the old model may have been destroyed

	if {$resizeBinding ne ""} {
	    catch {uplevel #0 [list $plotModel systembind canvas delete $resizeBinding]}
	    set resizeBinding ""
	}
	
	set inModelInit TRUE

	next $Model

	$controller setModel $Model
	
	if {$Model ne ""} {
	    set resizeBinding\
		[uplevel #0 [list $plotModel systembind canvas add <Configure>\
				 "[self] updateViewport"]]
	    set modelMap [set [set [info object namespace $Model]::widgetview]::map]
	} else {
	    set modelMap ""
	}
	
	set inModelInit FALSE
	
	my updateRange
	my updateViewarea
	my MoveDecorations
    }
    
    ## Use Worldview Visuals instead of ordinary Visuals if they
    ## exist. The Scatterplot layer respective worldview uses always
    ## circle glyphs with the same size. 
    method AddLayer {layer} {
	my variable canvas map visuals plotModel
	

	set type [$plotModel layer getType $layer]

	switch -- $type {
	    scatterplot -
	    graph -
	    histogram -
	    graph -
	    points -
	    text -
	    texts {
		## Use custom worldview visuals
		dict set bboxStateBindings $layer\
		    [$plotModel systembind state add {x y xTemp yTemp} "[self] updateRange"]
		
		set cmd [format "::loon::classes::%sWorldviewVisual"\
			     [string toupper $type 0]]

		dict set visuals $layer\
		    [$cmd new [$plotModel layer getObject $layer] [self] $layer $canvas $map]

	    }
	    default {
		dict set bboxStateBindings $layer\
		    [$plotModel layer use $layer systembind state add {x y} "[self] updateRange"]
		
		next $layer
	    }	    
	}

	if {!$inModelInit} {
	    my updateRange
	}
    }

    ## Keep layers at correct rendering order
    method MoveLayer {layer} {
	next $layer
	my MoveDecorations
    }

    method MoveDecorations {} {
	variable canvas
	uplevel #0 [list $canvas lower "LoonWorldBox"]
	uplevel #0 [list $canvas lower "LoonViewareaFill"]
	uplevel #0  [list $canvas lower "LoonViewareaFill"]
	uplevel #0 [list  $canvas lower "LoonModelDataFill"]
	uplevel #0 [list $canvas raise "LoonViewareaOutline"]
    }

    
    method DeleteLayer {layer} {
	my variable plotModel
	
	set needUpdateRange FALSE
	if {[dict exists $bboxStateBindings $layer]} {
	    
	    if {$layer eq "model"} {
		## if destroyed
		catch {$plotModel systembind state delete\
			   [dict get $bboxStateBindings $layer]}
	    } else {
		## the layer is deleted anyway and no bindings get sent
		## anymore
		#$plotModel layer use $layer systembind state delete\
		#    [dict get $bboxStateBindings $layer]
	    }
	    dict unset bboxStateBindings $layer
	    
	    set needUpdateRange TRUE
	}
	
	next $layer

	if {$needUpdateRange && !$inModelInit} {
	    my updateRange
	}
    }

    ## Keep the deltaX and deltaY updated to be the range of all
    ## layers
    method updateRange {} {
	my variable plotModel map deltaX_var deltaY_var
	
	if {$plotModel eq ""} {
	    $map setDeltaX 1
	    $map setDeltaY 1
	    $map setPanX 0
	    $map setPanY 0
	    set bbox {0 0 1 1}
	    set bboxmodel {0 0 1 1}
	} else {
	    set bbox [$plotModel layer bbox root]
	    if {[lindex $bbox 0] eq ""} {
		$map setDeltaX 1
		$map setDeltaY 1
		$map setPanX 0
		$map setPanY 0
		set bbox {0 0 1 1}		
	    } else {
		set deltaX [expr {[lindex $bbox 2] - [lindex $bbox 0]}]
		set deltaY [expr {[lindex $bbox 3] - [lindex $bbox 1]}]

		
		if {$deltaX < 0.0005} {
		    set deltaX [set $deltaX_var]
		    lset bbox 0 [expr {[lindex $bbox 0] - [set $deltaX_var]/2.0}]
		    ##::loon::warning "x ranges too small."
		}
		if {$deltaY < 0.0005} {
		    set deltaY [set $deltaY_var]
		    lset bbox 1 [expr {[lindex $bbox 1] - [set $deltaY_var]/2.0}]
		    
		}
		
		$map setDeltaX $deltaX
		$map setDeltaY $deltaY
		$map setPanX [lindex $bbox 0]
		$map setPanY [lindex $bbox 1]
	    }
	    set bboxmodel [$plotModel layer bbox "model"]		    
	    if {[lindex $bboxmodel 0] eq ""} {
		set bboxmodel $bbox
	    }
	}
	
	$worldBox setCoords {*}$bbox
	$modeldataFill setCoords {*}$bboxmodel
	my updateViewport
    }
    
    
    method updateViewport {} {
	my variable plotModel canvas_width canvas_height map canvas\
	    deltaX_var deltaY_var zoomX_var zoomY_var swap_var
	
	## Plot_View will not supply w and h

	if {$plotModel eq ""} {
	    next
	} else {
	    ## the worldview only works for widgets
	
	    
	    ## Worldview
	    set rng_x [$map getDeltaX]
	    set rng_y [$map getDeltaY]
	    
	    ## Scatterplot Widget
	    set pmw [$modelMap getPlotWidth]
	    set pmh [$modelMap getPlotHeight]
	    
	    set delta_x [set $deltaX_var]
	    set delta_y [set $deltaY_var]
	    set zoom_x [set $zoomX_var]
	    set zoom_y [set $zoomY_var]
	    set swap [set $swap_var]
	    
	    if {$swap} {
		set r [expr {double($delta_x)*$zoom_y*$pmw*$rng_y/($delta_y*$zoom_x*$pmh*$rng_x)}]
	    } else {
		set r [expr {double($delta_y)*$zoom_x*$pmw*$rng_x/($delta_x*$zoom_y*$pmh*$rng_y)}]
	    }

	    if {$canvas_pratio < $r} {
		## maximize width in canvas
		set pw [expr {$canvas_width - 40}]
		set ph [expr {double($pw)/$r}]
		
		set vpx0 20
		set vpx1 [expr {$canvas_width - 20}]
		
		set vpy0 [expr {($canvas_height-$ph)/2.0}]
		set vpy1 [expr {$canvas_height - $vpy0}]
	    } else {
		## maximize height in canvas
		set ph [expr {$canvas_height - 40}]
		set pw [expr {$ph*$r}]
		
		set vpy0 20
		set vpy1 [expr {$canvas_height - 20}]
		
		set vpx0 [expr {($canvas_width - $pw)/2.0}]
		set vpx1 [expr {$canvas_width - $vpx0}]
	    }
	    next [list $vpx0 $vpy0 $vpx1 $vpy1];# calls updateCoords
	}
	
    }
    
    method canvasResize {w h} {
	set canvas_pratio [expr {double($w-40)/($h-40)}]
	next $w $h
    }
    

    
}
