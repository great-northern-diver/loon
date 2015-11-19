

::oo::class create ::loon::classes::ButtonPressMotion_Controller {
    
    superclass ::loon::classes::Canvas_Controller
 
    variable mouse_x mouse_y

    constructor {view} {	
	set mouse_x 0
	set mouse_y 0
	
	next $view

    }
    
    method init {} {
	my variable canvas
	
	next
	
	bind $canvas <ButtonPress-1> "+[self] button1_press %x %y"
	bind $canvas <Button1-Motion> "+[self] button1_motion %x %y"
	bind $canvas <ButtonRelease-1> "+[self] button1_release %x %y"


	# Meta Keys for Bindings (copy from loon namespace in loon.tcl)
	if {$::tcl_platform(os) eq "Linux"} {
	    # on my machine Option is always pressed if NumLock is on
	    set metaKeys {Shift Control} ;# Alt Command 
	} elseif {$::tcl_platform(platform) eq "windows"} {
            set metaKeys {Shift Control} ;# Alt Command 
        } else {
	    set metaKeys {Command Shift Control Alt Option}
	}
	
	## Meta Keys
	foreach meta $metaKeys {
	    bind $canvas <$meta-Button-1> "+[self] meta_b1 %x %y $meta"
	    bind $canvas <$meta-Button1-Motion> "+[self] meta_b1_motion %x %y $meta"
	    
	    foreach meta2 $metaKeys {
		## otherwise get Control-Control-Button-1 instead
		## of Control-Button-1
		if {$meta ne $meta2} {
		    bind $canvas <$meta-$meta2-Button-1>\
			"+[self] meta_meta_b1 %x %y $meta $meta2"
		}
	    }
	}
	
    }

    
    ## for button 1, 2 & 3
    method save_coords {x y} {
	set mouse_x $x
	set mouse_y $y
    }
    
    method button1_press {x y} {
	my save_coords $x $y
    }
    
    method button1_motion {x y} {}
    
    method button1_release {x y} {}

    method meta_meta_b1 {x y meta1 meta2} {}
    
    method meta_b1_motion {x y meta} {}
 
    method meta_b1 {x y meta} {}   
    
    
}
