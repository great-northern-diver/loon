oo::class create loon::classes::PolygonGlyph {

    superclass  ::loon::classes::Glyph

    constructor {args} {

	next {*}$args

	## Coordinates for lines
	my New_state x nested_double n ""
	my New_state y nested_double n ""

	my New_state linewidth positive_integer n 1

	my New_state showArea boolean n 1

	my SetInitStates n {x y}

	## Coordinates for points
	#my New_state x_nodes nested_double n ""
	#my New_state y_nodes nested_double n ""

	#my New_state showNodes boolean 1 FALSE

	## scaling
	## x and y separately, together, none
	#my New_state scaling factor 1 separate {separate together none}

	#my New_state showEnclosing boolean 1 FALSE
	#my New_state bboxColor color 1 gray70

	my SetStateDescription x\
	    "nested list of x coordinates of the polygons"
	my SetStateDescription y\
	    "nested list of y coordinates of the polygons"

	my SetStateDescription linewidth\
	    "linewidth in pixel"

	my SetStateDescription showArea\
	    "if TRUE shows filled polygons, otherwise only ploygon outline"


    }



    method EvalConfigure {} {
	my variable confDict x y

	next


	if {[dict get $confDict has_x] || [dict get $confDict has_y]} {

	    if {[dict get $confDict has_x]} {
		set tmp_x [dict get $confDict arg_x]
	    } else {
		set tmp_x $x
	    }

	    if {[dict get $confDict has_y]} {
		set tmp_y [dict get $confDict arg_y]
	    } else {
		set tmp_y $y
	    }

	    if {[llength $tmp_x] > 0} {
		set i 0
		foreach tmp_x_el $tmp_x tmp_y_el $tmp_y {
		    if {[llength $tmp_x_el] ne [llength $tmp_y_el]} {
			error "nested element at index $i in x and y are not of the same length"
		    }
		    incr i
		}
	    }
	}
    }

}
