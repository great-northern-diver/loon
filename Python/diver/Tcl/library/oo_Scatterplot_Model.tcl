::oo::class create ::loon::classes::Scatterplot_Model {
    
    superclass ::loon::classes::XYPair_Model\
	::loon::classes::withLayers\
	::loon::classes::withGlyphs
    

    constructor {args} {
	next {*}$args

	my AddLayer model "scatterplot"\
	    [::loon::classes::ScatterplotLayer new [self]] root 0 "Scatterplot"
	
    }
    
    
    method InfoDebug {args} {
	next {*}$args
	
    }
    
     
    

    

}
