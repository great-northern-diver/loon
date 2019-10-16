
oo::class create loon::classes::TextGlyph {
    
    superclass ::loon::classes::Glyph

    variable text
    
    constructor {} {
	
	next
	
	my New_state text string any ""
	
	my SetInitStates n text

	my SetStateDescription string\
	    "text string to be displayed as a point glyph"
	
    }
    
}
