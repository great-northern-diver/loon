
::oo::class create ::loon::classes::PointsLayer {
    
    superclass ::loon::classes::PrimitiveLayerN

    constructor {Container} {
	my variable type
	
	next $Container FALSE
	
	set type "points"
	
	my New_state size double n 4

	my New_state color colorOrTransparent n\
	    [lindex $::loon::Options(colors) 0]
	my New_state linecolor colorOrTransparent n {}
	my New_state linewidth positive_double n 1


	my SetStateDescription size\
	    "sizes for points. Same size mapping is used as for scatterplot points."
	
	my SetStateDescription color\
	    "fill color of points"

	my SetStateDescription linecolor\
	    "outline colors of points"
	
	my SetStateDescription linewidth\
	    "linewidths of outlines"

	
    }
    

}
