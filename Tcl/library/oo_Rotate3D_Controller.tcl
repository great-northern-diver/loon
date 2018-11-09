

::oo::class create ::loon::classes::Rotate3D_Controller {
    
    superclass ::loon::classes::Canvas_Controller
    
    variable map

    constructor {view} {
	
	#set mouse_x 0
	#set mouse_y 0
	
	set ns [info object namespace $view]
	set map [set [uplevel #0 ${ns}::my varname map]]

	next $view
	
    }


    method init {} {
	my variable view canvas

	next
	
	bind $canvas <ButtonPress-1> {+; focus %W}
    bind $canvas <w> "+[self] rotate3D up"
	bind $canvas <a> "+[self] rotate3D left"
    bind $canvas <s> "+[self] rotate3D down"
    bind $canvas <d> "+[self] rotate3D right"

    }
    
    method rotate3D {direction} {
	my variable model canvas
    
	if {$model eq ""} {return}

	
	## x and y are sometimes substituted wrong...
	#set wx [winfo pointerx $canvas]
	#set wy [winfo pointery $canvas]
	#set rx [winfo rootx $canvas]
	#set ry [winfo rooty $canvas]
	
	#set xn [expr {$wx-$rx}]
	#set yn [expr {$wy-$ry}]
	
	set dx 0
	set dy 0

    set updateStep 1
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
    
	#set mouse_x $xn
	#set mouse_y $yn
	#update idletasks
    }
    
}
