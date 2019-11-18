oo::class create ::loon::classes::Slicing2d {
    
    superclass ::loon::classes::withScaledData\
	::loon::classes::Context2d
    
    
    variable plot_xy plot_uv areDisplaysInit inDisplayUpdate\
	randomNumbers randomN xRect yRect
    
    constructor {Graph Navigator} {

	set areDisplaysInit FALSE
	set inDisplayUpdate FALSE

	set randomNumbers {}
	set randomN 0
	
	next $Graph $Navigator

	## epsilon is proportion * range of data
	my New_state proportion positive_double 1 0.1

	my New_state conditioning4d factor 1 "intersection"\
	    {intersection union sequential}
	
	my SetStateDescription proportion\
	    "proportion of the range of a variable that defines the neighborhood for slicing."

	my SetStateDescription conditioning4d\
            "specifies the conditioning method with 4d edge transitions and has to be either 'union', 'intersection' or 'sequential'"
	
	
	
    }
    
    method HookAfterStatesSet {} {
	next
	my variable changedStates

	if {"data" in $changedStates ||\
		"scaling" in $changedStates ||\
		"proportion" in $changedStates ||\
		"conditioning4d" in $changedStates} {
	    my Update
	}
    }
    
    method Update {} {
	my variable n data curXvars curYvars curP scaled_data curFrom curTo\
	    proportion

	if {$n eq 0 || $scaled_data eq ""} {return}
	
	my EvalCommand

	
    }

    # active x y xlabel ylabel from to p
    method EvalCommand {} {
	my variable command substitutions
	
	# currently only our implementation
	uplevel #0 $command
	
	if {FALSE && [llength $command] ne 0} {
	    uplevel #0 [string map [list {*}$substitutions\
					%a [list $active]\
				        %xlabel [list $xlabel]\
					%ylabel [list $ylabel]\
					%x [list $x]\
					%y [list $y]\
					%from $from %to $to %p $p] $command]
	}
    }




    ## Maintain Both Scatterplot Displays
   
    method CreateDisplays {} {
	set plot_xy [::loon::plot -title "see here as active"]
	set plot_uv [::loon::plot -title "slice here"]

	$plot_uv systembind state add selected\
	    [list [self namespace]::my UpdateSelected]

	$plot_xy systembind state add destroy\
	    [list [self namespace]::my DisplayDelete]
	$plot_uv systembind state add destroy\
	    [list [self namespace]::my DisplayDelete]

	set xRect [$plot_uv layer add rectangle -x {0 0} -y {0 0}\
		       -linecolor "#393B66" -color "" -linewidth 4]
	set yRect [$plot_uv layer add rectangle -x {0 0} -y {0 0}\
		       -linecolor "#393B66" -color "" -linewidth 4]
	
	set areDisplaysInit TRUE	
    }

    method DisplayDelete {} {
	set areDisplaysInit FALSE
    }


    method UpdateSelected {} {
	if {!$areDisplaysInit} {return}
	
	$plot_xy configure -active [$plot_uv cget -selected]
	
	if {!$inDisplayUpdate} {
	    ## make rectangle dissapear
	    $plot_uv layer hide $xRect
	    $plot_uv layer hide $yRect 	    
	}
	
    }

    ## cache random numbers for same n
    method GetRandomNumbers {num} {
	if {$num eq $randomN} {
	    return $randomNumbers
	} else {
	    set randomNumbers {}
	    for {set i 0} {$i < $num} {incr i} {
		lappend randomNumbers [expr {rand()}]
	    }
	    set randomN $num
	    return $randomNumbers
	}
    }
    
    
    method updateDisplays {} {
	if {!$areDisplaysInit} {return}

	my variable n curXvars curYvars curP scaled_data curFrom curTo\
	    proportion conditioning4d

       	if {[llength $curXvars] eq 0 || [llength $curYvars] eq 0} {return}
	
	set inDisplayUpdate TRUE

	set uCondition FALSE
	set vCondition FALSE
	
	set x_label [lindex $curXvars 0]
	set x_var [dict get $scaled_data [lindex $curXvars 0]]
	if {[llength $curXvars] eq 2} {
	    set u_var [dict get $scaled_data [lindex $curXvars 1]]
	    set u_label [lindex $curXvars 1]
	    set uCondition TRUE
	}
	
	set y_label [lindex $curYvars 0]
	set y_var [dict get $scaled_data [lindex $curYvars 0]]
	if {[llength $curYvars] eq 2} {
	    set v_var [dict get $scaled_data [lindex $curYvars 1]]
	    set v_label [lindex $curYvars 1]
	    set vCondition TRUE
	} 
      
	
	if {$uCondition || $vCondition} {
	    if {!$uCondition} {
		set u_var [my GetRandomNumbers [llength $x_var]]
		set u_label "random numbers"
	    }
	    if {!$vCondition} {
		set v_var [my GetRandomNumbers [llength $y_var]]
		set v_label "random numbers"
	    }	    
	} else {
	    ## No points
	    ## Temporary fix
	    set u_var [my GetRandomNumbers [llength $x_var]]
	    set v_var [my GetRandomNumbers [llength $y_var]]
	    #	    set u_var 1
	    #	    set v_var 1
	    set u_label "x = same random numbers as y"
	    set v_label "y = same random numbers as x"
	    set selected TRUE
	}

	if {$conditioning4d eq "sequential"} {
	    if {$curP < 0.5} {
		set uCondition TRUE
		set vCondition FALSE
		set curP [expr {2.0*$curP}]
	    } else {
		set uCondition FALSE
		set vCondition TRUE
		set curP [expr {($curP - 0.5)*2}]
	    }
	    set setOperation intersection
	} else {
	    set setOperation $conditioning4d
	}
	
	set mmu [::loon::listfns::MinMax $u_var]
	set mmv [::loon::listfns::MinMax $v_var]

	set p2 [expr {$proportion/2.0}]
	
	if {$uCondition} {
	    set minu [lindex $mmu 0]
	    set maxu [lindex $mmu 1]
	    set du [expr {$maxu - $minu}]
	    
	    set x0 [expr {$minu + max(0, $curP - $p2)*$du}]
	    set x1 [expr {$minu + min(1, $curP + $p2)*$du}]

	    $plot_uv layer use $xRect configure\
		-x [list $x0 $x1] -y $mmv
	    
	    $plot_uv layer show $xRect
	} else {
	    $plot_uv layer use $xRect configure\
		-x $mmu -y $mmv
	    $plot_uv layer hide $xRect
	}

	if {$vCondition} {
	    set minv [lindex $mmv 0]
	    set maxv [lindex $mmv 1]
	    set dv [expr {$maxv - $minv}]
	    
	    set y0 [expr {$minv + max(0, $curP - $p2)*$dv}]
	    set y1 [expr {$minv + min(1, $curP + $p2)*$dv}]

	    $plot_uv layer use $yRect configure\
		-x $mmu -y [list $y0 $y1]
	    
	    $plot_uv layer show $yRect
	} else {
	    $plot_uv layer use $yRect configure\
		-x $mmu -y $mmv
	    $plot_uv layer hide $yRect
	}

	
	switch -- $setOperation {
	    union {
		set selected [lrepeat [llength $x_var] FALSE]

		if {$uCondition} {
		    set i 0
		    foreach u $u_var {
			if {$u >= $x0 && $u <= $x1} {
			    lset selected $i TRUE
			}
			incr i
		    }
		}

		if {$vCondition} {
		    set i 0
		    foreach v $v_var {
			if {$v >= $y0 && $v <= $y1} {
			    lset selected $i TRUE
			}
			incr i
		    }
		}
	    }
	    intersection {
		set selected [lrepeat [llength $x_var] TRUE]

		if {$uCondition} {
		    set i 0
		    foreach u $u_var {
			if {$u < $x0 || $u > $x1} {
			    lset selected $i FALSE
			}
			incr i
		    }
		}

		if {$vCondition} {
		    set i 0
		    foreach v $v_var {
			if {$v < $y0 || $v > $y1} {
			    lset selected $i FALSE
			}
			incr i
		    }
		}		
	    }
	    default {
		error "wrong set operation: $setOperation"
	    }	    
	}
	

	
	$plot_xy configure -x $x_var -y $y_var -xlabel $x_label -ylabel $y_label
	$plot_uv configure -x $u_var -y $v_var -xlabel $u_label -ylabel $v_label\
	    -selected $selected

	
	$plot_xy scaleto world
	$plot_uv scaleto world
	
	set inDisplayUpdate FALSE	
    }
    
    
    
}
