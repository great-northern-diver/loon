
::oo::class create ::loon::classes::OvalLayer {
    
    superclass ::loon::classes::PrimitiveLayer1

    constructor {Container} {
	my variable type
	
	next $Container

	set type "oval"

	my New_state color colorOrTransparent 1 ""
	my New_state linecolor colorOrTransparent 1 steelblue
	my New_state linewidth positive_double 1 1


	my SetStateDescription color\
	    "fill color of oval"

	my SetStateDescription linecolor\
	    "outline colors of oval"
	
	my SetStateDescription linewidth\
	    "linewidth of outline"
	
    }
    
}
