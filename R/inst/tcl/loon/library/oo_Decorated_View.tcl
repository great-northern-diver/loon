

::oo::class create ::loon::classes::Decorated_View {

    superclass ::loon::classes::Layered_View

    variable dv_soi border labels scalesGuides\
	swapAxes_var\
	xlabel_var ylabel_var title_var\
	showScales_var showLabels_var showGuides_var\
	foreground_var background_var\
	guidesBackground_var guidelines_var\
	minimumMargins_var labelMargins_var scalesMargins_var
    
    constructor {args} {
	my variable canvas map

	## states of interest
	set dv_soi {xlabel ylabel title swapAxes showScales showGuides showLabels\
			foreground background guidesBackground guidelines\
			minimumMargins labelMargins scalesMargins}
	
	foreach state $dv_soi {
	    set ${state}_var ""
	}
	
	next {*}$args

	set border [::loon::classes::BorderVisual new "border" $canvas $map]
	
	set labels [::loon::classes::LabelsVisual new "label" $canvas $map]
	
	set scalesGuides [::loon::classes::ScalesGuidesVisual new\
			      "scale" $canvas $map]
	
    }
    
    method InfoDebug {args} {
	next border labels scalesGuides {*}$args
    }

    ## Keep layers at correct rendering order
    method MoveLayer {layer} {
	variable canvas
			
	next $layer

	uplevel #0 [list $canvas raise "loon&&border"]
	uplevel #0 [list  $canvas raise "loon&&label"]
	uplevel #0  [list $canvas raise "loon&&scale"]
	uplevel #0 [list $canvas lower "loon&&scale&&guideline"]
    }

    method setPlotModel {Model} {
	my variable canvas

	if {$Model ne ""} {
	    
	    set ns [info object namespace $Model] 
	    foreach state $dv_soi {
		set ${state}_var [uplevel #0 [list ${ns}::my varname $state]]
	    }

	    set showScales [set $showScales_var]
	    set showGuides [set $showGuides_var]
	    set showLabels [set $showLabels_var]

	    $scalesGuides setShowScales $showScales
	    $scalesGuides setShowGuides [set $showGuides_var]
	    $scalesGuides setGuidelinesColor [set $guidelines_var]

	    $labels setLabels [set $title_var] [set $xlabel_var] [set $ylabel_var]
	    $labels setMargins {*}[set $labelMargins_var]
	    
	    if {$showScales || $showGuides} {
		$scalesGuides setVisibility TRUE
	    } else {
		$scalesGuides setVisibility FALSE
	    }
	    
	    if {$showLabels} {
		$labels setVisibility TRUE
	    } else {
		$labels setVisibility FALSE
	    }
	    
	    if {$showScales || $showLabels} {
		$border setVisibility TRUE
	    } else {
		$border setVisibility FALSE
	    }
	    
	    if {$showGuides} {
		$canvas configure -bg [set $guidesBackground_var]
	    } else {
		$canvas configure -bg [set $background_var]
	    }	
	    
	    
	    ## initialize decoraction
	    #$border redraw
	    #$labels redraw
	    #$scalesGuides redraw
    	}
	
	next $Model
    }

    ## add labels, scales and guides as layers
    

    method redraw {} {
	next
	$border redraw
	$labels redraw
	$scalesGuides redraw
    }
    
    method updateCoords {} {
	next 
	$border updateCoords
	$labels updateCoords
	$scalesGuides updateScales
    }

    method updatePan {args} {
	next {*}$args
	$scalesGuides updateScales
    }

    
    method updateZoomPan {args} {
	next {*}$args
	$scalesGuides updateScales
    }

    
    method plotUpdateDict {events} {
	my variable canvas
	## need to get swap set
	next $events
	
	## swap causes an updateCoords call anyway
	#if {[dict exists $events "swapAxes"]} {
	#    $labels updateCoords
	#}

	set needViewportUpdate FALSE
	if {[dict exists $events "title"] || [dict exists $events "xlabel"] || \
		[dict exists $events "ylabel"]} {
	    $labels setLabels [set $title_var] [set $xlabel_var] [set $ylabel_var]
	    set needViewportUpdate TRUE
	}
	
	if {[dict exists $events "labelMargins"]} {
	    $labels setMargins {*}[set $labelMargins_var]
	    set needViewportUpdate TRUE
	}
	
	if {[dict exists $events "scalesMargins"] || [dict exists $events "minimumMargins"]||\
	    [dict exists $events "swapAxes"] } {
	    set needViewportUpdate TRUE
	}

	
	set hasShowScales [dict exists $events "showScales"]
	set hasShowGuides [dict exists $events "showGuides"]
	set hasShowLabels [dict exists $events "showLabels"]
       	

	set showGuides [set $showGuides_var]
	
	if {$hasShowGuides || $hasShowScales || $hasShowLabels} {

	    set showScales [set $showScales_var]
	    set showLabels [set $showLabels_var]
	      
	    ## Set visibility
	    if {$showScales || $showGuides} {
		$scalesGuides setVisibility TRUE
	    } else {
		$scalesGuides setVisibility FALSE
	    }
	    
	    if {$showLabels} {
		$labels setVisibility TRUE
	    } else {
		$labels setVisibility FALSE
	    }
	    
	    if {$showScales || $showLabels} {
		$border setVisibility TRUE
	    } else {
		$border setVisibility FALSE
	    }


	    
	    if {$hasShowScales} {
		$scalesGuides setShowScales $showScales
	    }
	    if {$hasShowGuides} {
		$scalesGuides setShowGuides $showGuides		
		if {$showGuides} {
		    $canvas configure -bg [set $guidesBackground_var]
		    $scalesGuides updateCoords		   
		} else {
		    $canvas configure -bg [set $background_var]
		}	
	    }
	    set needViewportUpdate TRUE
	}
	
	if {$needViewportUpdate} {
	    my updateViewport
	}
	
	if {$showGuides && [dict exists $events "guidesBackground"]} {
	    $canvas configure -bg [set $guidesBackground_var]
	}
	
	if {[dict exists $events "background"] || [dict exists $events "foreground"]\
		|| [dict exists $events "guidelines"]} {

	    set bg [set $background_var]
	    set fg [set $foreground_var]

	    $border setColor $bg $fg
	    $labels setColor $fg
	    $scalesGuides setColor $fg
	    
	    $scalesGuides setGuidelinesColor [set $guidelines_var]
	}
    }
    

    ## the viewport is in canvas coordinates
    method updateViewport {} {
	my variable canvas_width canvas_height

	set vpx0 0; set vpx1 $canvas_width
	set vpy0 0; set vpy1 $canvas_height
	
	set marginTop 0
	set marginBottom 0
	set marginLeft 0
	set marginRight 0
	
	## bottom left top right
	## 0      1    2   3  
	## ()
	if {[set $showScales_var]} {
	    set scalesMargins [set $scalesMargins_var]
	    
	    incr marginLeft [lindex $scalesMargins 1]
	    incr marginBottom [lindex $scalesMargins 0]
	}
	
	if {[set $showLabels_var]} {
	    
	    set labelMargins [set $labelMargins_var]
	    
	    set swapAxes [set $swapAxes_var]

	    if {[set $title_var] ne ""} {
		incr marginTop [lindex $labelMargins 2]
	    }
	    if {[set $ylabel_var] ne ""} {
		if {$swapAxes} {
		    incr marginBottom [lindex $labelMargins 0]
		} else {
		    incr marginLeft [lindex $labelMargins 1]
		}
	    }
	    if {[set $xlabel_var] ne ""} {
		if {$swapAxes} {
		    incr marginLeft [lindex $labelMargins 1]
		} else {
		    incr marginBottom [lindex $labelMargins 0]
		}
	    }
	}
	
	
	if {[set $showScales_var] || [set $showLabels_var]} {
	    set minimumMargins [set $minimumMargins_var]
	    
	    if {$marginBottom < [lindex $minimumMargins 0]} {
		set marginBottom [lindex $minimumMargins 0]
	    } 
	    if {$marginLeft < [lindex $minimumMargins 1]} {
		set marginLeft [lindex $minimumMargins 1]
	    }
	    if {$marginTop < [lindex $minimumMargins 2]} {
		set marginTop [lindex $minimumMargins 2]
	    }
	    if {$marginRight < [lindex $minimumMargins 3]} {
		set marginRight [lindex $minimumMargins 3]
	    }
	}
	
	incr vpx0 $marginLeft
	incr vpy0 $marginTop
	incr vpx1 -$marginRight
	incr vpy1 -$marginBottom
		
	next [list $vpx0 $vpy0 $vpx1 $vpy1]
	
	$border updateCoords
	$labels updateCoords
    }

    method canvasResize {w h} {
	my variable canvas
	next $w $h
	$border updateCoords
	$labels updateCoords
	#$canvas raise labels	
    }


}
