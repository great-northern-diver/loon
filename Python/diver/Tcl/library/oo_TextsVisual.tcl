
::oo::class create ::loon::classes::TextsVisual { 

    ## because of fast zooming and panning
    superclass ::loon::classes::PointsVisual

    variable text_var anchor_var justify_var angle_var


    constructor {Layerobj args} {
	
	set ns [info object namespace $Layerobj]	
	foreach state {text anchor justify angle} {
	    set ${state}_var [uplevel #0 [list ${ns}::my varname $state]]
	}
	
	next $Layerobj {*}$args	
    }


    method redraw {} {
	my variable canvas ids n_var
	
	if {$ids ne "noinit"} {
	    my clear
	}
	

	set ids {}
	for {set i 0} {$i < [set $n_var]} {incr i} { 
	    lappend ids [uplevel #0 [list $canvas create text 0 0]]
	}
	
	my updateActive
	my updateCoords
	my updateItems

	nextto ::loon::classes::LayerVisual ;## move layer into correct place	
    }

    
    method updateCoords {} {
	my variable canvas map isVisible ids n_var x_var y_var
	
	if {$ids eq "noinit" || [llength $ids] ne [set $n_var]} {
	    my redraw
	} else {
	    if {$isVisible} {
		set sxycoords [$map mapDxy2Sxy [set $x_var] [set $y_var]]
		
		foreach id $ids\
		    x [lindex $sxycoords 0] y [lindex $sxycoords 1] {
			uplevel #0 [list $canvas coords $id $x $y]
		    }
	    }
	}
    }
    
    
    method updateItems {} {
	my variable canvas ids size_var color_var
	
	if {$ids eq "noinit"} {
	    my redraw
	} else {

	    foreach id $ids\
		color [set $color_var]\
		text [set $text_var]\
		angle [set $angle_var]\
		anchor [set $anchor_var]\
		justify [set $justify_var]\
		size [set $size_var]\
		tag [my getTags] {
		    
		    set font  [format "%s %s"\
				   $::loon::Options(font)\
				   $size]
		    
		    uplevel #0 [list $canvas itemconfigure $id\
				    -tag $tag\
				    -text $text\
				    -fill $color\
				    -angle $angle\
				    -anchor $anchor\
				    -justify $justify\
				    -font $font]
		}
	}
    }
    
}
