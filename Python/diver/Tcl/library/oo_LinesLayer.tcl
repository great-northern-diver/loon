
::oo::class create ::loon::classes::LinesLayer {
    
    superclass ::loon::classes::PrimitiveLayerN

    constructor {Container} {
	my variable type
	
	next $Container
	
	set type "lines"

	my New_state color color n black
	my New_state dash nested_positive_double n ""
	my New_state linewidth positive_double n 1

	my SetStateDescription color\
	    "line colors"
		
	my SetStateDescription linewidth\
	    "line widths"

	my SetStateDescription dash\
	    "one dash pattern used by all line. Dash pattern is a list of integers representing number of pixels of repeated line segments. The odd segments are drawn with color, the even elements a drawn transparent."
    }
    
    
}
