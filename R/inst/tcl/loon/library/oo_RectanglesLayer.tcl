
::oo::class create ::loon::classes::RectanglesLayer {
    
    superclass ::loon::classes::PrimitiveLayerN

    constructor {Container} {
	my variable type
	
	next $Container
	
	set type "rectangles"

	my New_state color colorOrTransparent n black
	my New_state linecolor colorOrTransparent n ""
	my New_state linewidth positive_double n 1

	my SetStateDescription color\
	    "fill color of rectangles"

	my SetStateDescription linecolor\
	    "outline colors of rectangles"
	
	my SetStateDescription linewidth\
	    "linewidths of outlines"
    }

    
    method GetDefaultValue {state length} {	
	switch -- $state {
	    linecolor {
		return [lrepeat $length {}]
	    }
	    default {
		return [next $state $length]
	    }
	}
    }
    
    

    
}
