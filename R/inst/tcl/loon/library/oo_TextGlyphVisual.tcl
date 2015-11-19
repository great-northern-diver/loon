
oo::class create loon::classes::TextGlyphVisual {
    
    superclass ::loon::classes::GlyphVisual
        
    variable text_var

    constructor {GlyphObject args} {
	
	next $GlyphObject {*}$args
	
	set ns [info object namespace $GlyphObject]
	
	set text_var [${ns}::my varname text]
    }
    
    
    method draw {canvas ind} {
	return [$canvas create text 0 0\
		    -text [lindex [set $text_var] $ind]\
		    -anchor c -justify c]
    }
    
    method updateCoords {id canvas ind size} {
	$canvas coords $id 0 0
	
	set fontSize [::loon::map_text_size $size]
	$canvas itemconfigure $id -font "Lucidia $fontSize"
	
    }
    
    method recolor {id canvas ind color} {
	$canvas itemconfigure $id -fill $color
    }
    
    method InfoDebug {args} {
	next text_var {*}$args
    }
    
}
