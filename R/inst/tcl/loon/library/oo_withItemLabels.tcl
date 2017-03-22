
::oo::class create ::loon::classes::withItemLabels {

    constructor {args} {

	next {*}$args

	my New_state itemLabel string n ""
	my New_state showItemLabels boolean 1 FALSE

	my SetStateDescription itemLabel\
	    "item labels for each data point that are shown if showItemLabels=TRUE as a tooltip when hovering over a point visual"

	my SetStateDescription showItemLabels\
	    "boolean to specify whether to display the itemlabes when hovering over a point visual"
	
	
    }

    method GetDefaultValue {state length} {
	if {$state eq "itemLabel" || $state eq "tag"} {
	    if {$length eq 0} {return ""}
	    return [::loon::listfns::ljoin\
			[lrepeat $length "item"]\
			[::loon::listfns::lseq 0 [expr {$length-1}]]]
	} else {
	    return [next $state $length]
	}
    }
    
}
