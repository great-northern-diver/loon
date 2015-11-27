
oo::class create ::loon::classes::withScaledData {
    
    superclass ::loon::classes::withData
    
    variable scaled_data
    
    constructor {args} {

	set scaled_data ""
	
	next {*}$args
	
	my New_state scaling factor 1 "variable"\
	    [list variable none observation variable\
		 variable01 observation01 data01]

	my SetStateDescription scaling\
	    "one of 'none', 'observation', 'variable', 'variable01', 'observation01' or 'data01'. See web-documentation"
	
    }

    method HookAfterStatesSet {} {
	my variable changedStates scaling n numeric_data data
	
	## first evaluate data to get numeric_data
	next
	
	if {"data" in $changedStates ||\
		"scaling" in $changedStates} {
	    
	    if {$n eq 0} {
		set scaled_data {}
	    } else {
		switch -- $scaling {
		    none {
			set scaled_data $numeric_data
		    }
		    observation {
			## (xi-mean(x))/sd(x)
			set scaled_data [dict create]
			my variable n
			
			set keys [dict keys $numeric_data]
			
			for {set i 0} {$i < $n} {incr i} {
			    set obs {}
			    foreach key $keys {
				lappend obs [lindex [dict get $numeric_data $key] $i]
			    }
			    
			    set sobs [::loon::listfns::scale $obs]
			    
			    foreach key $keys o $sobs {
				dict lappend scaled_data $key $o
			    }
			}			
		    }
		    variable {
			set scaled_data [dict create]
			dict for {key value} $numeric_data {
			    dict set scaled_data $key [::loon::listfns::scale $value]
			}
		    }
		    observation01 {
			set scaled_data [dict create]
			my variable n
			
			set keys [dict keys $numeric_data]
			
			for {set i 0} {$i < $n} {incr i} {
			    set obs {}
			    foreach key $keys {
				lappend obs [lindex [dict get $numeric_data $key] $i]
			    }
			    
			    set sobs [::loon::listfns::scale01 $obs]
			    
			    foreach key $keys o $sobs {
				dict lappend scaled_data $key $o
			    }
			}
		    }
		    variable01 {
			set scaled_data [dict create]
			dict for {key value} $numeric_data {
			    dict set scaled_data $key [::loon::listfns::scale01 $value]
			}
		    }
		    data01 {
			set min {}
			set max {}
			dict for {key value} $numeric_data {
			    set mm [::loon::listfns::MinMax $value]
			    lappend min [lindex $mm 0]
			    lappend max [lindex $mm 1]
			}
			
			set min [::loon::listfns::min $min]
			set max [::loon::listfns::max $max]
			
			set scaled_data [dict create]
			dict for {key value} $numeric_data {
			    dict set scaled_data $key [::loon::listfns::scale01 $value $min $max]
			}
		    }
		}
		
	    }
	    
	}

	
    }
    
}
