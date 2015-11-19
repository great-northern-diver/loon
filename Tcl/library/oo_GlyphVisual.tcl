
oo::class create loon::classes::GlyphVisual {
    
    superclass ::loon::classes::fancyInfo

    variable glyphObject map
    
    constructor {GlyphObject Map args} {
	
	set map $Map
	set glyphObject $GlyphObject
	
	next
	
    }
    

    method InfoDebug {args} {
	next {*}$args
    }
    
    ## return id or list of ids
    # draw {canvas ind size color showArea tag}
    method draw {args} {}

    ## return nothing
    # updateCoords {id canvas ind size showArea}
    method updateCoords {args} {}
    # recolor {id canvas ind color showArea}
    method recolor {args} {}
    
}
