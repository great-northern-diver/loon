
::oo::class create ::loon::classes::PolygonsLayer {
    
    superclass ::loon::classes::PrimitiveLayerN
    
    constructor {Container} {
	my variable type
	
	next $Container
	
	set type "polygons"
	
	my New_state color colorOrTransparent n {}
	my New_state linecolor colorOrTransparent n black
	my New_state linewidth positive_double n 1


	my SetStateDescription color\
	    "fill colors of polygons"

	my SetStateDescription linecolor\
	    "outline colors of polygons"
	
	my SetStateDescription linewidth\
	    "linewidths of outlines"
	
    }
    
    
}
