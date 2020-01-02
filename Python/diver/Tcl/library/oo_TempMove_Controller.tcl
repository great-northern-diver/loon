

::oo::class create ::loon::classes::TempMove_Controller {
    
    superclass ::loon::classes::ButtonPressMotion_Controller

    variable tm_soi viewMap selected_var x_var y_var xTemp_var yTemp_var\
	zoomX_var zoomY_var deltaX_var deltaY_var swapAxes_var\
	hickupCount
    


    constructor {view} {
	## state of interest
	set tm_soi {selected selectBy selectionLogic\
			x y xTemp yTemp zoomX zoomY deltaX deltaY swapAxes}
	
	foreach state $tm_soi {
	    set ${state}_var ""
	}
	
	set viewMap [set [uplevel #0 [list [info object namespace $view]::my varname map]]]
	
	next $view
	
    }
    
    
    method setModel {Model} {
	
	my variable model

	set ns [info object namespace $Model] 
	foreach state $tm_soi {
	    set ${state}_var [uplevel #0 [list ${ns}::my varname $state]]
	}
	
	next $Model
	
    }


    method meta_b1_motion {x y meta} {
	my variable mouse_x mouse_y canvas

	if {$meta eq $::loon::Options(metaTempMove)} {

	    ## x and y are sometimes substituted wrong...
	    set wx [winfo pointerx $canvas]
	    set wy [winfo pointery $canvas]
	    set rx [winfo rootx $canvas]
	    set ry [winfo rooty $canvas]

	    set xn [expr {$wx-$rx}]
	    set yn [expr {$wy-$ry}]
	    
	    set dx [expr {$xn - $mouse_x}]
	    set dy [expr {$yn - $mouse_y}]

	    #if {$xn ne $x} {
	    #	puts "x=$x, xn=$xn"
	    #	puts "dx=$dx, dy=$dy"
	    #}
	    	    	    

	    
	    my tempMoveSelectedPoints $dx $dy
	    
	    my save_coords $xn $yn
	} else {
	    next $x $y $meta
	}
    }
    

    method tempMoveSelectedPoints {dx dy} {
	my variable model 
	
	if {$model eq ""} {return}

	
	set sel [set $selected_var]
	if {![::loon::listfns::any $sel]} {
	    return
	}
	
	set plot_width [$viewMap getPlotWidth]
	set plot_height [$viewMap getPlotHeight]
	
	set delta_x [set $deltaX_var]
	set delta_y [set $deltaY_var]
	set zoom_x [set $zoomX_var]
	set zoom_y [set $zoomY_var]
	set swap [set $swapAxes_var]
	
	## shift in data
	if {$swap} {
	    set ddx [expr {-double($dy)*$delta_x/$zoom_x/$plot_height}]
	    set ddy [expr {double($dx)*$delta_y/$zoom_y/$plot_width}]
	} else {
	    set ddx [expr {double($dx)*$delta_x/$zoom_x/$plot_width}]
	    set ddy [expr {-double($dy)*$delta_y/$zoom_y/$plot_height}]
	}

	if {[llength [set $xTemp_var]] eq 0} {
	    set x [set $x_var]
	}  else {
	    set x [set $xTemp_var]
	}
	if {[llength [set $yTemp_var]] eq 0} {
	    set y [set $y_var]
	}  else {
	    set y [set $yTemp_var]
	}
	
	set which [::loon::listfns::which $sel]
	
	foreach w $which {
	    set xtmp [lindex $x $w]
	    set ytmp [lindex $y $w]
	    lset x $w [expr {$xtmp + $ddx}]
	    lset y $w [expr {$ytmp + $ddy}]
	}
	
	$model configure -xTemp $x -yTemp $y
	update idletasks
    }
   
}
