
::oo::class create ::loon::classes::PrimitiveLayerN {
    
    superclass ::loon::classes::PrimitiveLayer
    
    variable isXYNested
    
    constructor {Container {xyNested TRUE}} {
	
	set isXYNested $xyNested
	
	next $Container

	if {$xyNested} {
	    my New_state x nested_double n ""
	    my New_state y nested_double n "" 
	} else {
	    my New_state x double n ""
	    my New_state y double n "" 
	}
	my SetInitStates n {x y}

	my New_state tag string n ""
	my New_state active boolean n TRUE

	my New_state itemLabel string n ""
	
	my SetStateDescription x\
	    "x coordinates"
	
	my SetStateDescription y\
	    "y coordinates"

	my SetStateDescription active\
	    "if TRUE visual is rendered, otherwise it is not"
	
	my SetStateDescription tag\
	    "tags useful for item bindings"

	
	
    }
    
    
    method GetDefaultValue {state length} {
	my variable type
	if {$state eq "tag"} {
	    if {$length eq 0} {return ""}
	    
	    return [::loon::listfns::ljoin\
			[lrepeat $length [string range $type 0 end-1]]\
			[::loon::listfns::lseq 0 [expr {$length-1}]]]
	    
	} elseif {$state eq "itemLabel"} {
	    if {$length eq 0} {return ""}
	    return [::loon::listfns::ljoin\
			[lrepeat $length "line item"]\
			[::loon::listfns::lseq 0 [expr {$length-1}]]]
	    
	} else {
	    return [next $state $length]
	}
    }

    method EvalConfigure {} {
	my variable confDict x y 
	
	next
	
	if {$isXYNested} {
	    if {[dict get $confDict has_x] || [dict get $confDict has_y]} {
		
		if {[dict get $confDict has_x]} {
		    set tmp_x [dict get $confDict arg_x]
		} else {
		    set tmp_x $x
		}
		
		if {[dict get $confDict has_y]} {
		    set tmp_y [dict get $confDict arg_y]
		} else {
		    set tmp_y $y
		}
		
		if {[llength $tmp_x] > 0} {
		    set i 0
		    foreach tmp_x_el $tmp_x tmp_y_el $tmp_y {		    
			if {[llength $tmp_x_el] ne [llength $tmp_y_el]} {
			    error "nested element at index $i in x and y are not of the same length"
			}
			incr i
		    }
		}
		
	    }
	}
    }

    
}
