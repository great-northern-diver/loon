

::oo::class create ::loon::classes::PrimitiveLayer {

    superclass ::loon::classes::Layer

    ## note this works with nested lists too
    method HookAfterStatesSet {} {
	my variable changedStates minX minY maxX maxY x y

	if {"x" in $changedStates} {
	    ## concat for nexted lists
	    set mm [::loon::listfns::MinMax [concat {*}$x]]
	    set minX [lindex $mm 0]
	    set maxX [lindex $mm 1]
	}

	if {"y" in $changedStates} {
	    set mm [::loon::listfns::MinMax [concat {*}$y]]
	    set minY [lindex $mm 0]
	    set maxY [lindex $mm 1]
	}

	next

    }

}
