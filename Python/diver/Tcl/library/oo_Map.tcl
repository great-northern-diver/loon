
oo::class create loon::classes::Map {
    
    superclass ::loon::classes::fancyInfo
    
    variable panX panY minX minY zoomX zoomY deltaX deltaY swap\
	vpx0 vpy0 vpx1 vpy1 plotHeight plotWidth rotate3DX rotate3DY
    
    constructor {} {
	set panX 0
	set panY 0
	set minX 0
	set minY 0
	set zoomX 1
	set zoomY 1
	set deltaX 1
	set deltaY 1
	set swap FALSE
	set vpx0 0
	set vpx1 1
	set vpy0 0
	set vpy1 1
	set plotWidth 1
	set plotHeight 1
    set rotate3DX 0
    set rotate3DY 0
    
    }
    
    method setPanX {panx} { set panX $panx }
    method setPanY {pany} { set panY $pany }
    method setRotate3DX {rotate3dx} { set rotate3DX $rotate3dx }
    method setRotate3DY {rotate3dy} { set rotate3DY $rotate3dy }
    method setMinX {minx} { set minX $minx }
    method setMinY {miny} { set minY $miny }
    method setZoomX {zoomx} { set zoomX $zoomx }
    method setZoomY {zoomy} { set zoomY $zoomy }
    method setDeltaX {rangex} { set deltaX $rangex }
    method setDeltaY {rangey} { set deltaY $rangey }
    method setSwap {Swap} { set swap $Swap }
    method setViewport {Vpx0 Vpy0 Vpx1 Vpy1} { 
	set vpx0 $Vpx0 
	set vpy0 $Vpy0
	set vpx1 $Vpx1
	set vpy1 $Vpy1
	set plotWidth [expr {$vpx1 - $vpx0}]
	set plotHeight [expr {$vpy1 - $vpy0}]
    }
    
    method getPlotWidth {} { return $plotWidth }
    method getPlotHeight {} { return $plotHeight }

    method getPanX {}   { return $panX }
    method getPanY {}   { return $panY }
    method getRotate3DX {}   { return $rotate3DX }
    method getRotate3DY {}   { return $rotate3DY }
    method getMinX {}   { return $minX }
    method getMinY {}   { return $minY }
    method getZoomX {}  { return $zoomX }
    method getZoomY {}  { return $zoomY }
    method getDeltaX {} { return $deltaX }
    method getDeltaY {} { return $deltaY }
    method getSwap {}   { return $swap }
    method getViewport {} {return [list $vpx0 $vpy0 $vpx1 $vpy1]}
    method getVpx0 {} { return $vpx0 }
    method getVpy0 {} { return $vpy0 }
    method getVpx1 {} { return $vpx1 }
    method getVpy1 {} { return $vpy1 }

    method info {} {
	next panX panY minX minY zoomX zoomY deltaX deltaY swap\
	    vpx0 vpy0 vpx1 vpy1 rotate3DX rotate3DY
    }

        
    ## D : Data
    ## S : Screen
    ## delta: {dx dy}
    
    method mapDxy2Sxy {x y} {
	if {$swap} {
	    return [list\
			[::loon::fastmap $y $zoomY $panY $deltaY $vpx0 $vpx1]\
			[::loon::fastmap $x $zoomX $panX $deltaX $vpy1 $vpy0]]
	} else {
	    return [list\
			[::loon::fastmap $x $zoomX $panX $deltaX $vpx0 $vpx1]\
			[::loon::fastmap $y $zoomY $panY $deltaY $vpy1 $vpy0]]
	}
    }
    
    method mapDxy2Scoords {x y} {
	if {$swap} {
	    return [::loon::fastmapXY2Coords $y $x $zoomY $zoomX $panY $panX\
			$deltaY $deltaX $vpx0 $vpx1 $vpy1 $vpy0]
	} else {
	    return [::loon::fastmapXY2Coords $x $y $zoomX $zoomY $panX $panY\
			$deltaX $deltaY $vpx0 $vpx1 $vpy1 $vpy0]
	}
    }
    
    method mapDcoords2Scoords {dcoords} {
	return [::loon::fastmapCoords2Coords $dcoords $swap\
		    $zoomX $zoomY $panX $panY $deltaX $deltaY $vpx0 $vpy0 $vpy1 $vpy1]
    }
    
    
    method mapS2D {sx sy} {
	
	if {$swap} {
	    set ydast [expr {double($sx - $vpx0)/$zoomY/$plotWidth*$deltaY + $panY}]
	    set xdast [expr {double($vpy1 - $sy)/$zoomX/$plotHeight*$deltaX + $panX}]
	} else {
	    set xdast [expr {double($sx - $vpx0)/$zoomX/$plotWidth*$deltaX + $panX}]
	    set ydast [expr {double($vpy1 - $sy)/$zoomY/$plotHeight *$deltaY + $panY}]
	}
	
	return [list $xdast $ydast]
    }

    method zoomPanUpdate {x y factor {direction both}} {
	if {$direction eq "both"} {
	    set factorX $factor
	    set factorY $factor   
	} elseif {$direction eq "x" && !$swap ||\
		      $direction eq "y" && $swap} {
	    set factorX $factor
	    set factorY 1	    
	} else {
	    set factorX 1
	    set factorY $factor	    	    	    
	}

	set newZoomX [expr {double($zoomX)*$factorX}]
	set newZoomY [expr {double($zoomY)*$factorY}]

	
	if {$swap} {
	    set ydast [expr {double($x - $vpx0)/$zoomY/$plotWidth*$deltaY + $panY}]
	    set xdast [expr {double($vpy1 - $y)/$zoomX/$plotHeight*$deltaX + $panX}]
	    
	    set newPanX [expr {$xdast - ($xdast-$panX)/double($factorX)}]
	    set newPanY [expr {$ydast - ($ydast-$panY)/double($factorY)}]
	} else {
	    set xdast [expr {double($x - $vpx0)/$zoomX/$plotWidth*$deltaX + $panX}]
	    set ydast [expr {double($vpy1 - $y)/$zoomY/$plotHeight *$deltaY + $panY}]
	    
	    set newPanX [expr {$xdast - ($xdast-$panX)/double($factorX)}]
	    set newPanY [expr {$ydast - ($ydast-$panY)/double($factorY)}]
	}
	
	return [list -panX $newPanX -panY $newPanY -zoomX $newZoomX -zoomY $newZoomY]	
    }

    method panUpdate {dx dy} {
	if {$swap} {
	    set newPanX [expr {$panX + double($dy)*$deltaX/$zoomX/$plotHeight}]
	    set newPanY [expr {$panY - double($dx)*$deltaY/$zoomY/$plotWidth}]
	} else {
	    set newPanX [expr {$panX - double($dx)*$deltaX/$zoomX/$plotWidth}]
	    set newPanY [expr {$panY + double($dy)*$deltaY/$zoomY/$plotHeight}]
	}
    
	return [list -panX $newPanX -panY $newPanY]
    }

    method dPan2dS {dPanX dPanY} {
	if {$swap} {
	    set dx [expr {double($zoomY) * $plotWidth/$deltaY*$dPanY}]	    
	    set dy [expr {double($zoomX) * $plotHeight/$deltaX*$dPanX*-1}]

	} else {
	    set dx [expr {double($zoomX) * $plotWidth/$deltaX*$dPanX}]
	    set dy [expr {double($zoomY) * $plotHeight/$deltaY*$dPanY*-1}]
	}
	return [list $dx $dy]
    }
    
    ## TODO:: encapsulate as a proc
    method dPanZoom2dS {x y oldPanX oldPanY oldZoomX oldZoomY} {
	set xs {}
	set ys {}
	
	if {$swap} {
	    set fx [expr {-1.0*double($plotHeight)/$deltaX}]
	    set fy [expr {-1.0*double($plotWidth)/$deltaY}]	    
	    
	    foreach xe $x ye $y {
		lappend ys [expr {$fx*(double($zoomX)*($xe - $panX)\
					   - double($oldZoomX)*($xe-$oldPanX))}]
		lappend xs [expr {$fy*(double($oldZoomY)*($ye-$oldPanY)\
					   - double($zoomY)*($ye - $panY))}]
	    }

	} else {
	    set fx [expr {double($plotWidth)/$deltaX}]
	    set fy [expr {double($plotHeight)/$deltaY}]

	    foreach xe $x ye $y {
		lappend xs [expr {$fx*(double($zoomX)*($xe - $panX)\
					   - double($oldZoomX)*($xe-$oldPanX))}]
		lappend ys [expr {$fy*(double($oldZoomY)*($ye-$oldPanY)\
					   - double($zoomY)*($ye - $panY))}]
	    }
	} 
	
	return [list $xs $ys]	
    }
    
    method rotate3DUpdate {dx dy} {
        set pi 3.1415926535897931
        set fraction 360
        if {$swap} {
            set newRotate3DX [expr {- $dx * $pi / $fraction}]
            set newRotate3DY [expr {- $dy * $pi / $fraction}]
        } else {
            set newRotate3DX [expr {$dy * $pi / $fraction}]
            set newRotate3DY [expr {$dx * $pi / $fraction}]
        }
        
        return [list -rotate3DX $newRotate3DX -rotate3DY $newRotate3DY]
    }

    
}
