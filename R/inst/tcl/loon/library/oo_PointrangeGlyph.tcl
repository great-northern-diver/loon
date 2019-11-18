
oo::class create ::loon::classes::PointrangeGlyph {
    
    superclass ::loon::classes::Glyph
    
    constructor {} {

	my variable isGlyphZoomSensitive
		
	next
	
	my New_state ymin double n 0 
	my New_state ymax double n 1
	my New_state linewidth positive_double n 1
	
	my New_state showArea boolean 1 TRUE
	
	my SetInitStates n {ymin ymax}
	
	set isGlyphZoomSensitive TRUE


	my SetStateDescription ymin\
	    "minimum of point range for each point"
	my SetStateDescription ymax\
	    "maximum of point range for each point"

	my SetStateDescription linewidth\
	    "line width of pointrange line"

	my SetStateDescription showArea\
	    "if TRUE shows a points at x/y with an outline circle glyph, if FALSE with a outlined circle"

    }

    method EvalConfigure {} {
	my variable confDict
	
	next 
	
	set has_ymin [dict get $confDict has_ymin]
	set has_ymax [dict get $confDict has_ymax]
	
	if {$has_ymin || $has_ymax} {
	    if {$has_ymin} {
		set t_ymin [dict get $confDict arg_ymin]
	    } else {
		my variable ymin
		set t_ymin $ymin
	    }
	    if {$has_ymax} {
		set t_ymax [dict get $confDict arg_ymax]
	    } else {
		my variable ymax
		set t_ymax $ymax
	    }
	    
	    foreach y0 $t_ymin y1 $t_ymax {
		if {$y0 > $y1} {
		    error "elements in -ymin must be always less\
                           or equal than their corresponding element\
                           -ymax. But $y0 > $y1."
		}
	    }
	    
	}	

    }
    
}
