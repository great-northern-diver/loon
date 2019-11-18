
oo::class create ::loon::classes::withData {
    
    superclass ::loon::classes::VariableDimensions 
    
    
    variable data numeric_data

    constructor {args} {
	set data ""
	set numeric_data ""

	next {*}$args
	
	my AddStates data
    }

    
    method EvalConfigureInit {} {
	my variable confDict n
	
	## Need initialize
	if {[dict get $confDict has_data]} {
	    
	    set arg_data [dict get $confDict arg_data]
	    
	    if {$arg_data eq ""} {
		set n_tmp 0
	    } else {
		## Check if data is valid
		set n_tmp ""
		dict for {name variable} $arg_data {
		    if {$n_tmp eq ""} {
			set n_tmp [llength $variable]
		    } else {
			set n_var [llength $variable] 
			if {$n_var ne $n_tmp} {
			    "-data has variable \"$variable\" with length \"$n_var\".\
                             However length $n_tmp is expected."
			}
		    }
		} 
	    }
	    dict set confDict init_n TRUE
	    dict set confDict new_n $n_tmp
	    dict set confDict new_data $arg_data
	}
    }
    
    
    method HookAfterStatesSet {} {
	my variable changedStates n
	
	if {"data" in $changedStates} {
	    if {$n eq 0} {
		set numeric_data {}
	    } else {
		## Convert to numeric data
		set numeric_data $data
		foreach var [dict keys $numeric_data] {
		    if {![::loon::listfns::isNumeric [dict get $numeric_data $var]]} {
			dict set numeric_data $var\
			    [::loon::listfns::toNumeric [dict get $numeric_data $var]]
		    }
		}
	    }
	}
	next
	
    }
    
}
