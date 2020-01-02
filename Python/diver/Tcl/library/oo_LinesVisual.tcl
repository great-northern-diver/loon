

## N Lines
::oo::class create ::loon::classes::LinesVisual { 
    
    superclass ::loon::classes::LayerVisualN
    
    variable color_var linewidth_var dash_var
    
    constructor {Layerobj args} {
	
	set ns [info object namespace $Layerobj]
	foreach state {color linewidth dash} {
	    set ${state}_var [uplevel #0 [list ${ns}::my varname $state]]
	}
	
	next $Layerobj {*}$args
    }

    method redraw {} {
	my variable canvas isVisible n_var ids
	
	if {$ids ne "noinit"} {
	    my clear
	}
	
	set ids {}
	for {set i 0} {$i < [set $n_var]} {incr i} { 
	    lappend ids [uplevel #0 [list $canvas create line 0 0 0 0]]
	}
	
	my updateActive
	my updateCoords
	my updateItems
	
	next ;## move layer into correct place
    }

    method updateItems {} {
	my variable canvas ids
	
	if {$ids eq "noinit"} {
	    my redraw
	} else {
	    foreach id $ids\
		color [set $color_var]\
		linewidth [set $linewidth_var]\
		dash [set $dash_var]\
		tag [my getTags] {
		    uplevel #0 [list $canvas itemconfigure $id\
				    -fill $color\
				    -width $linewidth\
				    -tag $tag\
				    -dash $dash]
		}
	}
    }
    
}
