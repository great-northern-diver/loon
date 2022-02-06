

::oo::class create ::loon::classes::Resize_Controller {
    
    superclass ::loon::classes::Controller

    
    method init {} {
	my variable view

	next
	
	set canvas [$view getCanvas]
	bind $canvas <Configure> "+[self] canvasResize %w %h"
	
    }
    
    
    ## default implementation
    method canvasResize {w h} {
	my variable view
	$view canvasResize $w $h
    }
    
}
