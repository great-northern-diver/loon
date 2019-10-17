
::oo::class create ::loon::classes::LineLayer {
    
    superclass ::loon::classes::PrimitiveLayer1

    constructor {Container} {
	my variable type
	
	next $Container

	set type "line"
	
	my New_state color color 1 black
	my New_state linewidth positive_double 1 1
	my New_state dash positive_integer any ""

	my SetStateDescription color\
	    "line color"
		
	my SetStateDescription linewidth\
	    "line width"

	my SetStateDescription dash\
	    "dash pattern for line as a list of integers representing number of pixels of repeated line segments. The odd segments are drawn with color, the even elements a drawn transparent."
	
    }

    
    
}
