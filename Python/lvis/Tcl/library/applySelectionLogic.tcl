
namespace eval ::loon {

    proc applySelectionLogic {selectionState logic} {
	
	set n [llength $selectionState]
	if {$n > 0} {
	    if {$logic eq "select"} {
		set out [lrepeat $n 1]
	    } elseif {$logic eq "deselect"} {
		set out [lrepeat $n 0]
	    } elseif {$logic eq "invert"} {
		set out [::loon::listfns::booleanNot $selectionState]
	    } else {
		error "no valid selection logic"
	    }
	} else {
	    set out {}
	}
	
	return $out
    }

}
