
oo::class create loon::classes::Glyph {
    
    superclass ::loon::classes::withStateBindings\
	::loon::classes::VariableDimensions

    variable isGlyphZoomSensitive

    constructor {args} {
	
	## When zooming does the glyph size change?
	set isGlyphZoomSensitive FALSE

	next {*}$args
	
    }

    method isGlyphZoomSensitive {} {
	return $isGlyphZoomSensitive
    }
     
}
