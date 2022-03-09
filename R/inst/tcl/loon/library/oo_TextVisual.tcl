
## A Single Rectangle
::oo::class create ::loon::classes::TextVisual { 
    
    superclass ::loon::classes::LayerVisual1
    
    variable color_var size_var text_var angle_var anchor_var justify_var
    
    constructor {Layerobj args} {

	set ns [info object namespace $Layerobj]	
	foreach state {color size text angle anchor justify} {
	    set ${state}_var [uplevel #0 [list ${ns}::my varname $state]]
	}
	
	next $Layerobj {*}$args	
    }

    
    method redraw {} {
	my variable canvas isVisible id
	
	if {$id ne "noinit"} {
	    my clear
	}
	
	
	if {$isVisible} {set state normal} else {set state hidden}
	
	set id [uplevel #0 [list $canvas create text 0 0\
				-state $state]]
	
	my updateCoords
	my updateItem
	
	next ;## move layer into correct place
    }

    
    method updateItem {} {
	my variable isVisible canvas id
	
	if {$id eq "noinit"} {
	    my redraw
	} else {
	    if {$isVisible} {
		
		set font [format "%s %s"\
			      $::loon::Options(font)\
			      [set $size_var]]
		
		uplevel #0 [list $canvas itemconfigure $id\
				-tag [my getTags]\
				-text [set $text_var]\
				-fill {*}[set $color_var]\
				-angle [set $angle_var]\
				-anchor [set $anchor_var]\
				-justify [set $justify_var]\
				-font $font]
	    }
	}
    }
    
}
