oo::class create loon::classes::SerialaxesGlyph {
    
    superclass  ::loon::classes::Serialaxes\
	::loon::classes::Glyph

    constructor {args} {
	my variable defaultValues
	
	next {*}$args

	my New_state showEnclosing boolean 1 FALSE
	my New_state bboxColor color 1 gray70
	my New_state axesColor color 1 gray70
	
	my SetStateDescription showEnclosing\
	    "circle (axesLayout=radial) or sqaure (axesLayout=parallel) to show bounding box/circle of the glyph (or showing unit circle or rectangle with height 1 if scaling=none)"

	my SetStateDescription bboxColor\
	    "color of bounding box/circle"

	my SetStateDescription axesColor\
	    "color of axes"	
	
    }
    
}
