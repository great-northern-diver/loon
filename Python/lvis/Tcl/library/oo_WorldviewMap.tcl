
oo::class create loon::classes::WorldviewMap {

    superclass ::loon::classes::Map
    
    variable model panX_var panY_var zoomX_var zoomY_var


    ## zoom & pan are kept constant at
    ## zoomX = 1, zoomY = 1
    ## panX = min(X), panY = min(Y) (over all layers)

    constructor {ZoomX ZoomY}  {
	my variable zoomX zoomY
	
	next
	set zoomX $ZoomX
	set zoomY $ZoomY
    }

    method setZoomX {value} {}
    method setZoomY {value} {}

    method setModel {Model} {
	set model $Model
	if {$model ne ""} {
	    set modelns [info object namespace $model]
	    
	    foreach var {panX panY zoomX zoomY} {
		set ${var}_var [uplevel #0 [list ${modelns}::my varname $var]]
	    }
	}
    }
    
    method panUpdate {dx dy} {
	my variable swap deltaX deltaY plotWidth plotHeight
	
	set panX [set $panX_var]
	set panY [set $panY_var]
	
	if {$swap} {
	    set newPanX [expr {$panX - double($dy)*$deltaX/$plotHeight}]
	    set newPanY [expr {$panY + double($dx)*$deltaY/$plotWidth}]
	} else {
	    set newPanX [expr {$panX + double($dx)*$deltaX/$plotWidth}]
	    set newPanY [expr {$panY - double($dy)*$deltaY/$plotHeight}]
	}
	
	return [list -panX $newPanX -panY $newPanY]
    }
    
    method zoomPanUpdate {x y factor {direction both}} {
	my variable swap deltaX deltaY plotWidth plotHeight vpx0 vpx1 vpy0 vpy1\
	    panX panY
	
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
	
	set zoomX [set $zoomX_var]
	set zoomY [set $zoomY_var]
	
	set newZoomX [expr {double($zoomX)*$factorX}]
	set newZoomY [expr {double($zoomY)*$factorY}]
	
	set modPanX [set $panX_var]
	set modPanY [set $panY_var]
	
	if {$swap} {
	    set ydast [expr {double($x - $vpx0)/$plotWidth*$deltaY + $panY}]
	    set xdast [expr {double($vpy1 - $y)/$plotHeight*$deltaX + $panX}]
	    
	    set newPanX [expr {$xdast - ($xdast-$modPanX)/double($factorX)}]
	    set newPanY [expr {$ydast - ($ydast-$modPanY)/double($factorY)}]
	} else {
	    set xdast [expr {double($x - $vpx0)/$plotWidth*$deltaX + $panX}]
	    set ydast [expr {double($vpy1 - $y)/$plotHeight *$deltaY + $panY}]
	    
	    set newPanX [expr {$xdast - ($xdast-$modPanX)/double($factorX)}]
	    set newPanY [expr {$ydast - ($ydast-$modPanY)/double($factorY)}]
	}
	
	return [list -panX $newPanX -panY $newPanY -zoomX $newZoomX -zoomY $newZoomY]	
    }


}
