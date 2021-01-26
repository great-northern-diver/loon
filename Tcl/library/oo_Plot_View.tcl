
## The Plot View's responsibility is to create the canvas and keep the
## following states of the the Plot Model in sync
##
## - zoom and pan
## - swap axes
## - map and viewport

::oo::class create ::loon::classes::Plot_View {

    superclass ::loon::classes::fancyInfo

    variable map mapns path canvas plotModel plotModelStateBinding\
	canvas_width canvas_height\
	panX_var panY_var zoomX_var zoomY_var deltaX_var deltaY_var swap_var\
	background_var


    constructor {Path} {

	my SetMap

	set plotModel ""
	set plotModelStateBinding ""

	set canvas_width 650
	set canvas_height 460

	foreach state {panX panY zoomX zoomY deltaX deltaY swap} {
	    set ${state}_var ""
	}

	## Cache current state
	## panX, panY, ...

	set path $Path


	my Make
	bind $canvas <Configure> "+[self] canvasResize %w %h"

	next

    }

    destructor {

	$map destroy

	## if not a loon widget (just a view)
	catch {uplevel #0 [list $plotModel systembind state delete $plotModelStateBinding]}

	next

	catch {destroy $canvas}
    }


    method InfoDebug {args} {
	next map canvas plotModel plotModelStateBinding {*}$args
    }

    method SetMap {} {
	set map [::loon::classes::PlotMap new]
	set mapns [info object namespace $map]

    }

    method Make {} {

        frame $path -class Loon

	set canvas [::tk::canvas ${path}.canvas\
			-width $canvas_width -height $canvas_height\
			-background $::loon::Options(background)]

	## pack ::.can does not work
	pack $canvas -fill both -expand TRUE

	## to omit scoping or the use of uplevel #0
	##if {[string range $canvas 0 1] ne "::"} {
	##    set canvas "::$canvas"
	##}

    }

    method getCanvas {} {
	return $canvas
    }


    method setPlotModel {Model} {

	if {$plotModelStateBinding ne ""} {
	    catch {$plotModel systembind state delete $plotModelStateBinding}
	    set plotModelStateBinding ""
	}

	set plotModel $Model

	if {$plotModel ne ""} {
	    set plotModelStateBinding [$plotModel systembind state add all "[self] plotUpdate %e"]
	    set ns [info object namespace $plotModel]

	    foreach state {panX panY zoomX zoomY deltaX deltaY} {
		set ${state}_var [uplevel #0 [list ${ns}::my varname $state]]
		$map set[string toupper $state 0] [set [set ${state}_var]]
	    }
	    set background_var [uplevel #0 [list ${ns}::my varname background]]
	    set swap_var [uplevel #0 [list ${ns}::my varname swapAxes]]
	    $map setSwap [set $swap_var]

	    uplevel #0 [list $canvas configure -background [set $background_var]]
	}
    }


    method updateZoomPan {args} {
	my updateCoords
	## if scales, change
    }

    method updatePan {dPanX dPanY} {
	my updateCoords
    }

    method updateCoords {} {}
    method redraw {} {}


    ## by default use full canvas
    method updateViewport {{viewport ""}} {
	if {[llength $viewport] eq 0} {
	    set viewport [list 20 20 [expr {$canvas_width-20}] [expr {$canvas_height-20}]]
	}

	$map setViewport {*}$viewport

	## if viewport changes needs to update coordinates of
	## visuals
	my updateCoords
    }

    ## gets called from controller
    method canvasResize {w h} {
	set canvas_width $w
	set canvas_height $h
	my updateViewport
    }

    method plotUpdate {Events} {

	## A couple of events in are not of interest
	## for the decendents of Plot_View
	if {$Events eq "selected" || $Events eq "color" ||\
		$Events eq "size" || $Events eq "active"} {
	    return
	}

	## Copy events in a dict
	set events [dict create]
	foreach e $Events {
	    dict set events $e 1
	}

	if {[dict exists $events destroy]} {
	    ## Model got destroyed
	    my ModelDestroyed
	    return
	}

	## For speed if only zoom and pan
	set nevents [llength $Events]
	set numHits 0

	## keep numHits to improve on performance

	set oldPanX [$map getPanX]
	if {[dict exists $events "panX"]} {
	    incr numHits
	    set newPanX [set $panX_var]
	    $map setPanX $newPanX
	} else {
	    set newPanX $oldPanX
	}

	set oldPanY [$map getPanY]
	if {[dict exists $events "panY"]} {
	    incr numHits
	    set newPanY [set $panY_var]
	    $map setPanY $newPanY
	} else {
	    set newPanY $oldPanY
	}

	if {$numHits >= $nevents} {
	    my updatePan [expr {$oldPanX - $newPanX}]\
		[expr {$oldPanY - $newPanY}]
	    return
	}


	set oldZoomX [$map getZoomX]
	if {[dict exists $events "zoomX"]} {
	    incr numHits
	    set newZoomX [set $zoomX_var]
	    $map setZoomX $newZoomX
	}

	set oldZoomY [$map getZoomY]
	if {[dict exists $events "zoomY"]} {
	    incr numHits
	    set newZoomY [set $zoomY_var]
	    $map setZoomY $newZoomY
	}

	if {$numHits > 0} {
	    my updateZoomPan $oldPanX $oldPanY $oldZoomX $oldZoomY
	}

	if {$numHits >= $nevents} {
	    ## only zoom and pan
	    return
	}

	if {[dict exists $events "background"]} {
	    uplevel #0 [list $canvas configure -background [set $background_var]]
	}


	my plotUpdateDict $events
    }


    ## for speed performance copy elements in a dict
    method plotUpdateDict {events} {

	set needCoordsUpdate [dict exists $events "needCoordsUpdate"]
	if {[dict exists $events "swapAxes"]} {
	    $map setSwap [set $swap_var]
	    set needCoordsUpdate TRUE
	}

	if {[dict exists $events "deltaX"]} {
	    $map setDeltaX [set $deltaX_var]
	    set needCoordsUpdate TRUE
	}

	if {[dict exists $events "deltaY"]} {
	    $map setDeltaY [set $deltaY_var]
	    set needCoordsUpdate TRUE
	}

	if {$needCoordsUpdate} {
	    my updateCoords
	}

    }




    ## The aspect ratio alpha is defined by the number of units on the
    ## y axis that have the equivalent screen length as the 1 unit in
    ## x direction
    ##
    ## if swap, then y axis shows in x-direction

    method aspect {args} {

	set rX [set $deltaX_var]
	set rY [set $deltaY_var]
	set zX [set $zoomX_var]
	set zY [set $zoomY_var]

	if {[set $swap_var]} {
	    set dy [$map getPlotWidth]
	    set dx [$map getPlotHeight]
	} else {
	    set dx [$map getPlotWidth]
	    set dy [$map getPlotHeight]
	}

	switch -- [llength $args] {
	    0 {
		set alpha [expr {double($dy)*$zY/$rY*$rX/($dx*$zX)}]
		return $alpha
	    }
	    1 {
		## set aspect ratio
		## change zoomY
		set alpha $args
		if {![string is double $alpha] || $alpha <= 0 } {
		    error "alpha is \"$alpha\" but needs to be a strictly positive number"
		}

		set zoomY [expr {double($alpha)*$rY/$dy*($dx*$zX)/$rX}]

		if {$plotModel ne ""} {
		    uplevel #0 [list $plotModel configure -zoomY $zoomY]
		}

	    }
	    default {
		error "too many arguments \"$args\", expected 0 or 1 numeric argument"
	    }
	}


    }

    method ModelDestroyed {} {
	## overwrite for world view
	my destroy
    }

}
