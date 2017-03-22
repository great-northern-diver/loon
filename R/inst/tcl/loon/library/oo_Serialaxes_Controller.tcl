

::oo::class create ::loon::classes::Serialaxes_Controller {
    
    superclass ::loon::classes::ItemLabel_Controller

    variable sweep modelVisual
    
    constructor {view} {
	array set sweep {}

	set modelVisual ""
	
	next $view
	
    }
    
    
    method init {} {
	my variable canvas
	## Bindings
	bind $canvas <ButtonPress-1> "+[self] sweep_start %x %y"
	bind $canvas <Button1-Motion> "+[self] sweep_motion %x %y"
	bind $canvas <ButtonRelease-1> "+[self] sweep_end %x %y"

	bind $canvas <Shift-ButtonPress-1> "+[self] sweep_start %x %y TRUE"
	bind $canvas <Shift-Button1-Motion> "+[self] sweep_motion %x %y TRUE"
	bind $canvas <Shift-ButtonRelease-1> "+[self] sweep_end %x %y TRUE"

	## Invert Axis Direction
	#bind $canvas <Control-ButtonPress-1> "+[self]  axes_rearrange_start %x %y"
	#bind $canvas <Control-Button1-Motion> "+[self] axes_rearrange_motion %x %y"
	#bind $canvas <Control-ButtonRelease-1> "+[self]  axes_rearrange_end %x %y"

	next 
	
    }


    method setModelVisual {visual} {
	set modelVisual $visual
    }

      
    method sweep_start {x y {permanent FALSE}} {
	my variable model 
	set sweep(x0) $x
	set sweep(y0) $y
	set sweep(id) ""
	if {!$permanent} {
	    $model configure -selected FALSE
	}
    }
    
    method sweep_motion {x y {permanent FALSE}} {
	my variable canvas model

	set coords [list $sweep(x0) $sweep(y0) $x $y]
	
	if {$sweep(id) eq ""} {
	    set sweep(id) [$canvas create line\
			       $coords -fill $::loon::Options(brush_color)\
			       -width 2]
	} else {
	    $canvas coords $sweep(id) $coords
	}

	set ind [$modelVisual itemIndicesIntersectWithLine {*}$coords]
	
	if {$permanent} {
	    set selected [$model cget -selected]
	} else {
	    set selected [lrepeat [$model cget -n] FALSE]
	}
	
	foreach i $ind {
	    lset selected $i TRUE
	}

	$model configure -selected $selected
	
	uplevel #0 [list update idletasks]
    }
    
    method sweep_end {x y {permanent FALSE}} {
	my variable canvas
	$canvas delete $sweep(id)
    }

    
    method axes_rearrange_start {x y} {
	puts "axis rearrange start"
    }
    
    method axes_rearrange_motion {x y} {
	puts "axis rearrange motion"
    }
    method axes_rearrange_end {x y} {
	puts "axis rearrange end"
    }
    
    method axis_flip {x y} {
	my variable canvas
	
	#set idaxis [$canvas find closest $x $y 0 "loon_axis"]
	puts "flix axis"
	
    }

    
}
