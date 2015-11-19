
namespace eval loon {

    ## these procedures are implemented here to possibly gain 
    ## byte compilation advantages, or to later implement them in C
    
    ## also, these functions assume the origin to lie on the left
    ## bottom corner and not on the left upper corner as it is
    ## implemented for the canvas widget

    ## here the calling method needs to take care whether the coordinates are
    ## in y direction and call
    ## 
    ## [fastmap $x $zoom $pan $range $vpy1 $vpy0] 
    ##
    ## as opposed to  
    ##
    ## [fastmap $x $zoom $pan $range $vpx0 $vpx1] 
    ##
    ##  Tk Canvas:
    ##  -----------------------------
    ##  |  (vpx0,vpy0)              |
    ##  |      ........             |
    ##  |      |      |             |
    ##  |      |      |             |
    ##  |      ........             |
    ##  |           (vpx1,vpy1)     |
    ##  -----------------------------
    ##
    ##  These functions:
    ##  -----------------------------
    ##  |            (x1,y1)        |
    ##  |      ........             |
    ##  |      |      |             |
    ##  |      |      |             |
    ##  |      ........             |
    ##  |  (x0,y0)                  |
    ##  -----------------------------
    ##

    proc fastmap {x zoom pan range x0 x1} {
	
	set factor [expr {double($zoom)*($x1 - $x0)/$range}]

	set out {}
	foreach e $x {
	    lappend out [expr {$x0 + ($e - $pan)*$factor}]
	}
	return $out
    }
    
    ## here x and y stand for the axis direction and not for data if
    ## axes are swapped then the calling function has to arrange the
    ## arguments correctly
    proc fastmapXY2Coords {x y zoomX zoomY panX panY deltaX deltaY x0 x1 y0 y1} {

	set factorX [expr {double($zoomX)*($x1 - $x0)/$deltaX}]
	set factorY [expr {double($zoomY)*($y1 - $y0)/$deltaY}]
	
	
	set out {}
	foreach xe $x ye $y {
	    lappend out [expr {$x0 + ($xe - $panX)*$factorX}]
	    lappend out [expr {$y0 + ($ye - $panY)*$factorY}]
	}

	return $out
    }
    
    ## NOTE: here we need to know if the data needs to be swapped so
    ## zoomX, panX etc the x/y relates to the data x/y
    proc fastmapCoords2Coords {coords swap zoomX zoomY panX panY deltaX deltaY x0 y0 x1 y1} {
	
	set out {}
	
	if {$swap} {
	    set factorY [expr {double($zoomY)*($x1 - $x0)/$deltaY}]
	    set factorX [expr {double($zoomX)*($y1 - $y0)/$deltaX}]

	    
	    foreach {x y} $coords {
		lappend out [expr {$y0 + ($y - $panY)*$factorY}]
		lappend out [expr {$x0 + ($x - $panX)*$factorX}]
	    }

	} else {
	    set factorX [expr {double($zoomX)/($x1 - $x0)/$deltaX}]
	    set factorY [expr {double($zoomY)/($y1 - $y0)/$deltaY}]
	    
	    foreach {x y} $coords {
		lappend out [expr {$x0 + ($x - $panX)*$factorX}]
		lappend out [expr {$y0 + ($y - $panY)*$factorY}]
	    }
	}
	
	return $out
    }
    
}
