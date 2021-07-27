
oo::class create ::loon::classes::Serialaxes_Model {

    superclass ::loon::classes::Serialaxes\
	::loon::classes::withItemLabels\
	::loon::classes::withLayers\
	::loon::classes::Linkable


    constructor {args} {

	next {*}$args

	# method New_state {name type dim init args} ...
	my New_state active       boolean n TRUE
	my New_state color        color n [lindex  $::loon::Options(colors) 0]
	my New_state selected     boolean n FALSE

	my New_state showGuides   boolean 1 TRUE

	my New_state showLabels     boolean 1 TRUE

	my New_state useLoonInspector boolean 1 TRUE

	my New_state title        string 1 ""
	my New_state tag          string n ""

	# Maybe in the future add zoom and pan support
	# my New_state bbox double 4 {0 0 1 1}
	# my New_state panX double 1 0
	# my New_state panY double 1 0
	# my New_state zoomX positive_double 1 1
	# my New_state zoomY positive_double 1 1
	# my New_state deltaX double 1
	# my New_state deltaY double 1


	my setLinkedStates [list active color selected]

	my SetStateDescription showGuides\
	    "boolean to specify whether to show lines for visual guidance or not"
	my SetStateDescription showLabels\
	    "boolean to specify whether to display the axes labels and title or not"
	my SetStateDescription useLoonInspector\
	    "boolean to specify whether to report interaction events to the loon inspector or not"
	my SetStateDescription title\
	    "plot title"

	my SetStateDescription color\
	    "colors of the point glyphs"
	my SetStateDescription selected\
	    "selected points are highlighted and can be modified with the inspector"
	my SetStateDescription active\
	    "active points get rendered, inactive ones do not"
	my SetStateDescription tag\
	    "point glyphs have tags associated that can be used for item bindings"

    my AddLayer model "serialaxes"\
	    [::loon::classes::SerialaxesLayer new [self]] root 0 "Serialaxes"
    }


    method GetDefaultValue {state length} {
	if {$state eq "tag"} {
	    if {$length eq 0} {
		return {}
	    } else {
		set tags {}
		for {set i 0} {$i < $length} {incr i} {
		    lappend tags glyph$i
		}
		return $tags
	    }
	} else {
	    next $state $length
	}
    }



    method EvalConfigure {} {
	my variable confDict


	next

	## Now apply state rules
	## only active points can be selected
	set hasActive [dict exists $confDict new_active]
	set hasSelected [dict exists $confDict new_selected]
	if { $hasActive || $hasSelected } {

	    if {$hasActive} {
		set act [dict get $confDict new_active]
	    } else {
		my variable active
		set act $active
	    }

	    if {$hasSelected} {
		set sel [dict get $confDict new_selected]
	    } else {
		my variable selected
		set sel $selected
	    }

	    set needChange FALSE
	    foreach a $act s $sel {
		if {$s && !$a} {
		    set needChange TRUE
		    break
		}
	    }

	    set newSelected $sel
	    set i 0
	    foreach a $act s $sel {
		if {$s && !$a} {
		    lset newSelected $i FALSE
		}
		incr i
	    }
	    dict set confDict new_selected $newSelected
	}


    }




}
