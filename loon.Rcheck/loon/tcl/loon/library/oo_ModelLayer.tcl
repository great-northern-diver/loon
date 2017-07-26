
::oo::class create ::loon::classes::ModelLayer {
    
    superclass ::loon::classes::Layer

    variable modelns

    constructor {Container} {
	
	## we need to work with the namespace as the model
	## probably gets renamed if it is a widget
	set modelns [info object namespace $Container]

	next $Container
	
    }
    
    method getMinX {} {return [${modelns}::my cget -minX]}

    method getMinY {} {return [${modelns}::my cget -minY]}

    method getMaxX {} {return [${modelns}::my cget -maxX]}

    method getMaxY {} {return [${modelns}::my cget -maxY]}
    
    
#    ## redirect calls to the model
#    ## as currently the model is also the container
#    method unknown {args} {
#	${modelns}::my {*}$args
#    }
    
}
