

::oo::class create ::loon::classes::Canvas_Controller {
    
    superclass ::loon::classes::Controller

    variable canvas
    
    constructor {view} {
	
	set canvas [$view getCanvas]
	
	next $view
	
    }
}
