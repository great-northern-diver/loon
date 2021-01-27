

::oo::class create ::loon::classes::LineVisual {

    superclass ::loon::classes::LayerVisual1

    variable color_var linewidth_var dash_var n_var

    constructor {Layerobj args} {

	## n-1 is here number of lines
	set ns [info object namespace $Layerobj]
	foreach state {color linewidth dash n} {
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

	set id [uplevel #0 [list $canvas create line 0 0 0 0 -state $state]]

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
		uplevel #0 [list $canvas itemconfigure $id\
				-fill [set $color_var]\
				-width [set $linewidth_var]\
				-dash [set $dash_var]\
				-tag [my getTags]]
	    }
	}
    }

}
