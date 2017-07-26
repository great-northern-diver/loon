

::oo::class create ::loon::classes::GroupLayer {
    
    superclass ::loon::classes::Layer

    constructor {Container} {
	
	next $Container
	
	my variable type
	set type "group"

    }
    
    method getMinX {} {
	error "to be implement"
    }
    method getMaxX {} {
	error "to be implement"
    }

    method getMinY {} {
	error "to be implement"
    }

    method getMaxX {} {
	error "to be implement"
    }
    
}
