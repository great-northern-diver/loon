

::oo::class create ::loon::classes::ZoomPan_Controller {
    
    superclass ::loon::classes::Canvas_Controller
    
    variable mouse_x mouse_y zoomOutFactor zoomInFactor map

    constructor {view} {
	
	set mouse_x 0
	set mouse_y 0
	
	set zoomOutFactor [expr {1/1.1}]
	set zoomInFactor 1.1
	
	set ns [info object namespace $view]
	set map [set [uplevel #0 [list ${ns}::my varname map]]]

	next $view
	
    }


    method init {} {
	my variable view canvas

	next
	
	# Always save where the mouse was pressed last
	# for pan
	foreach i {1 2 3} {
	    bind $canvas <ButtonPress-$i> "+[self] zp_button %x %y"
	}

	
	# Zoom 
	## zoom in (linux)
	bind $canvas <Button-4> "+[self] zoom %x %y +1 both"
	bind $canvas <Button-5> "+[self] zoom %x %y -1 both"

	bind $canvas <Control-Button-4> "+[self] zoom %x %y +1 x"
	bind $canvas <Control-Button-5> "+[self] zoom %x %y -1 x"

	## zoom Windows, osx
	bind $canvas <MouseWheel> "+[self] mousewheel %x %y %D both"

	
	## Linux
#	switch -- $::tcl_platform(os) {
#	    Linux {
		bind $canvas <Shift-Button-4> "+[self] zoom %x %y +1 y"
		bind $canvas <Shift-Button-5> "+[self] zoom %x %y -1 y"
#	    }
#	    Darwin -
#	    "Windows NT" {
		bind $canvas <Control-MouseWheel> "+[self] mousewheel %x %y %D x"
		bind $canvas <Shift-MouseWheel> "+[self] mousewheel %x %y %D y"
#	    }	    
#	}

	
	#bind $canvas <Command-Button-4> "+[self] zoom %x %y +1 y"
	#bind $canvas <Command-Button-5> "+[self] zoom %x %y -1 y"

	## OSX & R & Non-Apple Keyboards
	if {$::tcl_platform(os)  eq "Darwin"} {
	    bind $canvas <Option-Button-4> "+[self] zoom %x %y +1 y"
	    bind $canvas <Option-Button-5> "+[self] zoom %x %y -1 y"
	    bind $canvas <Option-Button2-Motion> "+[self] pan %x %y y"
	    bind $canvas <Option-Button3-Motion> "+[self] pan %x %y y"
	}
	
	
	# Pan
	# left button click OSX
	bind $canvas <Button2-Motion> "+[self] pan %x %y both"
	bind $canvas <Button3-Motion> "+[self] pan %x %y both"

	bind $canvas <Control-Button2-Motion> "+[self] pan %x %y x"
	bind $canvas <Control-Button3-Motion> "+[self] pan %x %y x"

	bind $canvas <Shift-Button2-Motion> "+[self] pan %x %y y"
	bind $canvas <Shift-Button3-Motion> "+[self] pan %x %y y"

	
    }
    
    method zp_button {x y} {
	set mouse_x $x
	set mouse_y $y
    }

    method mousewheel {x y D direction} {
	if {$D > 0} {
	    my zoom $x $y +1 $direction
	} else {
	    my zoom $x $y -1 $direction
	}	
    }

    method zoom {x y diff direction} {	
	my variable model

	if {$model eq ""} {return}
	
	if {$diff > 0} {
	    set factor $zoomInFactor
	} else {
	    set factor $zoomOutFactor
	}
	
	$model configure {*}[$map zoomPanUpdate $x $y $factor $direction]

	update idletasks
    }
    
    method pan {x y {direction "both"}} {
	my variable model canvas
	if {$model eq ""} {return}

	
	## x and y are sometimes substituted wrong...
	set wx [winfo pointerx $canvas]
	set wy [winfo pointery $canvas]
	set rx [winfo rootx $canvas]
	set ry [winfo rooty $canvas]
	
	set xn [expr {$wx-$rx}]
	set yn [expr {$wy-$ry}]
	
	set dx [expr {$xn - $mouse_x}]
	set dy [expr {$yn - $mouse_y}]

	switch -- $direction {
	    both {
		$model configure {*}[$map panUpdate $dx $dy]
	    }
	    x {
		$model configure {*}[$map panUpdate $dx 0]
	    }
	    y {
		$model configure {*}[$map panUpdate 0 $dy]
	    }
	}

	set mouse_x $xn
	set mouse_y $yn
	update idletasks
    }
    
}
