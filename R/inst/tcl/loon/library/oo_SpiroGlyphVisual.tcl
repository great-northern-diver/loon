
oo::class create loon::classes::SpiroGlyphVisual {
    
    superclass  ::loon::classes::GlyphVisual
    
    variable x_var y_var n_var linewidth_var
    
    constructor {GlyphObject args} {

	set ns [info object namespace $GlyphObject]
	set x_var [${ns}::my varname x]
	set y_var [${ns}::my varname y]
	set n_var [${ns}::my varname n]
	set linewidth_var [${ns}::my varname linewidth]

	next $GlyphObject {*}$args
	
    }
    
    method draw {canvas ind} {
	my variable glyphObject
	
	
	set id [uplevel #0 [list $canvas create line 0 0 0 0\
		    -width [lindex [set $linewidth_var] $ind]]]
	
	return $id
    }
    
    
    method updateCoords {id canvas ind size} {
	my variable glyphObject

	set factor [::loon::map_spiro_size $size]
	
	$canvas coords $id\
	    {*}[::loon::listfns::interleave\
		    [lmap x [lindex [set $x_var] $ind] {expr {$x*$factor}}]\
		    [lmap y [lindex [set $y_var] $ind] {expr {$y*$factor}}]]	
	
    }


    method recolor {id canvas ind color} {
	$canvas itemconfigure $id -fill $color
    }    
    
}
