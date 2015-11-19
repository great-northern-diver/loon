
::oo::class create ::loon::classes::withItemlabels {

    constructor {args} {

	next {*}$args

	my New_state itemlabel string n ""
	my New_state showItemlabels boolean 1 FALSE

	my SetStateDescription itemlabel\
	    "item labels for each data point that are shown if showItemlabels=TRUE as a tooltip when hovering over a point visual"

	my SetStateDescription showItemlabels\
	    "boolean to specify whether to display the itemlabes when hovering over a point visual"
	
	
    }

    method GetDefaultValue {state length} {
	if {$state eq "itemlabel" || $state eq "tag"} {
	    if {$length eq 0} {return ""}
	    return [::loon::listfns::ljoin\
			[lrepeat $length "item"]\
			[::loon::listfns::lseq 0 [expr {$length-1}]]]
	} else {
	    return [next $state $length]
	}
    }
    
}
