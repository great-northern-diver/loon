
oo::class create loon::classes::PolygonGlyphVisual {

    superclass  ::loon::classes::GlyphVisual

    variable x_var y_var n_var linewidth_var showArea_var

    constructor {GlyphObject args} {

	set ns [info object namespace $GlyphObject]
	set x_var [${ns}::my varname x]
	set y_var [${ns}::my varname y]
	set n_var [${ns}::my varname n]
	set linewidth_var [${ns}::my varname linewidth]
	set showArea_var [${ns}::my varname showArea]

	next $GlyphObject {*}$args

    }

    method draw {canvas ind} {
	my variable glyphObject


	set id [uplevel #0 [list $canvas create polygon 0 0 0 0\
		    -width [lindex [set $linewidth_var] $ind]]]

	return $id
    }


    method updateCoords {id canvas ind size} {
	my variable glyphObject

	set factor [::loon::map_polygon_size $size]

    # polygon x coordinate
    set polygonx [lindex [set $x_var] $ind]
    # polygon y coordinate
    set polygony [lindex [set $y_var] $ind]

    if {![::loon::listfns::isNumeric $polygonx]} {

	   foreach polyx $polygonx polyy $polygony {
	      $canvas coords $id\
	      {*}[::loon::listfns::interleave\
		    [lmap x $polyx {expr {$x*$factor}}]\
		    [lmap y $polyy {expr {$y*$factor}}]]
	   }

	} else {
		$canvas coords $id\
	    {*}[::loon::listfns::interleave\
		    [lmap x $polygonx {expr {$x*$factor}}]\
		    [lmap y $polygony {expr {$y*$factor}}]]
	}
	}


    method recolor {id canvas ind color} {
	if {[lindex [set $showArea_var] $ind]} {
	    $canvas itemconfigure $id -fill $color -outline $color
	} else {
	    $canvas itemconfigure $id -fill "" -outline $color
	}
    }

}
