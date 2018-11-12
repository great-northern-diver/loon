

::oo::class create ::loon::classes::Rotate3D_Controller {
    
    superclass ::loon::classes::Canvas_Controller
    
    variable map updateStep

    constructor {view} {
	
	set ns [info object namespace $view]
	set map [set [uplevel #0 ${ns}::my varname map]]
    
    set updateStep 1

	next $view
	
    }


    method init {} {
	my variable view canvas

	next
	
	bind $canvas <ButtonPress-1> {+; focus %W}
    bind $canvas <Key-plus> "+[self] increaseSpeed"
    bind $canvas <Key-minus> "+[self] decreaseSpeed"
    bind $canvas <w> "+[self] rotate3D up"
	bind $canvas <a> "+[self] rotate3D left"
    bind $canvas <s> "+[self] rotate3D down"
    bind $canvas <d> "+[self] rotate3D right"

    }
    
    method increaseSpeed {} {
        incr updateStep
    }
    
    method decreaseSpeed {} {
        if {$updateStep > 1} {incr updateStep -1 }
    }
    
    method rotate3D {direction} {
	my variable model canvas
    
	if {$model eq ""} {return}
	
	set dx 0
	set dy 0
	switch -- $direction {
	    up {
        set dy $updateStep
	    }
	    down {
		set dy [expr -$updateStep]
	    }
	    left {
		set dx $updateStep
	    }
        right {
        set dx [expr -$updateStep]
        }
	}
    $model configure {*}[$map rotate3DUpdate $dx $dy]
    puts stdout [$map rotate3DUpdate $dx $dy]
    
    }
    
}
