

::oo::class create ::loon::classes::Layer {
    
    superclass ::loon::classes::withStateBindings\
	::loon::classes::VariableDimensions
    
    variable container type minX minY maxX maxY

    constructor {Container} {
	
	set type "??"

	set container $Container

	set minX ""
	set maxX ""
	set minY ""
	set maxY ""
	
	next

    }

    method getMinX {} {return $minX}

    method getMaxX {} {return $maxX}

    method getMinY {} {return $minY}

    method getMaxY {} {return $maxY}
    
    method getBbox {} {return [list\
				   [my GetMinX]\
				   [my GetMinY]\
				   [my GetMaxX]\
				   [my GetMaxY]]}
    
}
