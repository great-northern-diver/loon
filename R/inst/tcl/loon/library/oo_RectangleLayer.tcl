

::oo::class create ::loon::classes::RectangleLayer {
    
    superclass ::loon::classes::PrimitiveLayer1
    ## careful x and y have dim 2

    constructor {Container} {
	my variable type
	
	next $Container

	set type "rectangle"

	my New_state color colorOrTransparent 1 ""
	my New_state linecolor colorOrTransparent 1 steelblue
	my New_state linewidth positive_double 1 1


	my SetStateDescription color\
	    "fill color of rectangle"
	
	my SetStateDescription linecolor\
	    "outline colors of rectangle"
	
	my SetStateDescription linewidth\
	    "linewidth of outline"

	
	
    }
    
}
