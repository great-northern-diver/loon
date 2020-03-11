
::oo::class create ::loon::classes::VariableDimensions {

    superclass ::loon::classes::Configurable

    variable n defaultValues stateDimInfo

    ## The VariableDimensions class has the following responsibilities
    ##  - process the -which argument for n dimensional states
    ##  - change of cetain states trigger re-initialization of other states


    constructor {args} {

	## for states with a variable dimension, e.g. n, p, q, etc...
	set stateDimInfo\
	    [dict create n [dict create\
				isFixed	FALSE\
				fixedDim ""\
				initStates ""\
				states ""]]


	array set defaultValues {}

	## if a type with another dim variable is added it will be
	## added too.
	set n 0

	next {*}$args

    }

    method InfoDebug {args} {
	next {*}[dict keys $stateDimInfo] {*}$args
    }

    ## By default, n,p,q,... -dim states use the init state
    ## as default value
    method New_state {name type dim init args} {

	next $name $type $dim $init {*}$args

	if {$dim ne "any" && ![string is integer $dim]} {


	    ## initialize variable
	    my variable $name
	    set $name {}


	    my AddStatesToVariableDimension $name $dim
	    if {[llength $init] eq 0} {
		my AddDefaultValue $name {}
	    } else {
		my AddDefaultValue $name [my Check_$type init$name $init 1 {*}$args]
	    }

	}
    }


    method AddStatesToVariableDimension {states dim} {
	if {[dict exists $stateDimInfo $dim]} {
	    dict set stateDimInfo $dim states\
		[concat [dict get $stateDimInfo $dim states] {*}$states]
	} else {
	    my variable $dim
	    set $dim 0
	    dict set stateDimInfo $dim\
		[dict create\
		     isFixed FALSE\
		     dim ""\
		     initStates ""\
		     states $states]
	}
    }

    method SetInitStates {dim states} {
	dict set stateDimInfo $dim initStates $states

    }

    method GetStatesWithDim {dim} {
	return [dict get $stateDimInfo $dim states]
    }

    method configure {args} {

	## Preprocess -which and related args before they are sent to
	## configure method of Configurable class.  In future
	## -which_q, -which_p work too, -which_n is equivalent to -which
	## for now -which relates to n-dim states

	## [expr {"-which" in ...}] is faster than [lsearch -exact ...]
	set whichDims {}
	foreach varDim [dict keys $stateDimInfo] {
	    set checkStates FALSE
	    if {$varDim eq "n"} {
		if {"-which" in $args || "-which_n" in $args} {
		    set checkStates TRUE
		}
	    } else {
		if {"-which_$varDim" in $args} {
		    set checkStates TRUE
		}
	    }

	    if {$checkStates} {
		set hasFound FALSE
		foreach state [dict get $stateDimInfo $varDim states] {
		    if {"-$state" in $args} {
			lappend whichDims $varDim
			set hasFound TRUE
			break
		    }
		}

		## if no states are specified for which_* then throw a warning
		if {!$hasFound} {
		    puts "warning: -which_$varDim specified but no $varDim dimensional\
                          state submitted."

		    ## remove -which value elements
		    set iWhich [lsearch -exact $args "-which_$varDim"]
		    if {$varDim eq "n" && $iWhich eq "-1"} {
			set iWhich [lsearch -exact $args "-which"]
		    }
		    set iWhichIncr [expr {$iWhich + 1}]
		    set args [lreplace $args $iWhich $iWhichIncr]
		}

	    }
	}




	## process and apply -which_...
	foreach varDim $whichDims {

	    set iWhich [lsearch -exact $args "-which_$varDim"]

	    ## for n -which and -which_n are valid...
	    if {$varDim eq "n" && $iWhich eq "-1"} {
		set iWhich [lsearch -exact $args "-which"]
	    }

	    set iWhichIncr [expr {$iWhich + 1}]
	    set arg_which [lindex $args $iWhichIncr]


	    ## remove -which value elements
	    set args [lreplace $args $iWhich $iWhichIncr]

	    set which [my ProcessWhich $arg_which $varDim]
	    foreach state [dict get $stateDimInfo $varDim states] {

		if {"-$state" in $args} {
		    my variable $state

		    set tmp [set $state]

		    set i [lsearch -exact $args "-$state"]
		    incr i

		    set arg_state [lindex $args $i]

		    ## for states of type color, if the argument values
		    ## are not valid color names then the color mapping needs
		    ## to happen now, otherwise the previous color gets
		    ## overwritten
		    if {[my info stateType $state] eq "color"} {
			if {![::loon::listfns::isColor $arg_state]} {
			    set arg_state [::loon::listfns::mapColor $arg_state]
			}
		    }

		    if {[llength $arg_state] eq 1} {
			   foreach w $which {
			     lset tmp $w $arg_state
			   }
		    } elseif {[llength $arg_state] ne [llength $which]} {
			  error "-which applies to -$state and their lengths do not match.\
                                    Length 1 or [llength $which] is expected."
		    } else {
			   foreach w $which e $arg_state {
			      lset tmp $w $e
			   }
		    }

		    lset args $i $tmp
		}
	    }
	}

	next {*}$args

    }


    ## This method returns the dimension for n, p, q, etc..
    ## while within the configure method.
    method GetConfigureDim {qualifier} {
	my variable confDict

	if {[dict get $stateDimInfo $qualifier isFixed]} {
	    return [dict get $stateDimInfo $qualifier fixedDim]
	} elseif {[dict exists $confDict new_$qualifier]} {
	    return [dict get $confDict new_$qualifier]
	} else {
	    my variable $qualifier
	    return [set $qualifier]
	}
    }

    method Check_Dimensionality {var values dim} {

	if {[string is integer $dim]} {
	    return [next $var $values $dim]
	} elseif {$dim eq "any"} {
	    return $values
	}

	## is a qualifier n, p, q, etc...
	## used in configure
	set n_expected  [my GetConfigureDim $dim]
	set n_var [llength $values]

	if {$n_var eq 1 && $n_expected ne 0} {
	    set values [lrepeat $n_expected $values]
	} elseif {$n_var ne $n_expected} {
	    if {$n_expected eq 0} {
		error "-$var has length $n_var but length $n_expected is expected."
	    } else {
		error "-$var has length $n_var but length 1 or $n_expected is expected."
	    }
	}
	return $values
    }





    method EvalConfigureInit {} {
	my variable confDict

	## Need to initialize n,p,q,... dim states?
	foreach dim [dict keys $stateDimInfo] {

	    ## maybe init_$dim has been set by child class
	    set initStates [dict get $stateDimInfo $dim initStates]

	    if {![dict exists $confDict init_$dim] && $initStates ne ""} {

		my variable $dim ;# for current dimension

		## Check if initStates are all submitted and have the same dimension
		set new_dim ""
		set has_initState FALSE
		foreach initState $initStates {
		    if {[dict get $confDict has_$initState]} {
			set has_initState TRUE
			lappend new_dim [llength [dict get $confDict arg_$initState]]
		    }
		}

		if {$has_initState} {
		    set old_dim [set $dim]

		    ## Check if any of the initState dimensions differ
		    set dimHasChanged FALSE
		    foreach d $new_dim {
			if {$d ne $old_dim} {
			    set dimHasChanged TRUE
			    break
			}
		    }

		    if {$dimHasChanged} {
			if {[llength $initStates] ne [llength $new_dim]} {
			    error [format "To re-initialize $dim-dimensional states the following\
                                            states must be changed together: %s."\
				       [join $initStates ", "]]
			}

			if {[llength [lsort -unique $new_dim]] ne 1} {
			    error [format "The states that can re-initialize $dim-dimensional\
                                  states (i.e. %s) have different speciefied dimensions: %s."\
				       [join $initStates ", "] [join $new_dim ", "]]
			} else {
			    set new_dim [lindex $new_dim 0]
			}
		    } else {
			set new_dim [lindex $new_dim 0]
		    }
		}


		## now check if the dim of the initStates is different
		## from the current dim
		if {$has_initState && $new_dim ne [set $dim]} {

		    ## Maybe dimension is fixed
		    if {[dict get $stateDimInfo $dim isFixed] && \
			    [dict get $stateDimInfo $dim fixedDim] ne $new_dim} {
			## also if has fixed dim, then this has to be checked too

			if {[llength $initStates] eq 1} {
			    set txt state
			} else {
			    set txt states
			}
			error [format "Illegal length $new_dim for any of the $txt %s.\
                                       Length is fixed to %s."\
				   [join $initStates ", "]\
				   [dict get $stateDimInfo $dim fixedDim]]

		    }

		    dict set confDict init_$dim TRUE
		    dict set confDict new_$dim $new_dim
		}
	    }
	    ## If init, re-initialize all states that have the same dimension
	    ## and are not part of initStates

	    set hasInit [dict exists $confDict init_$dim]
	    if {$hasInit} {
		set isInit [dict get $confDict init_$dim]
	    }

	    if {[dict exists $confDict init_$dim] &&\
		    [dict get $confDict init_$dim]} {

		## EvalConfigureInit Pre  VariableDimensions
		##  - if was_init eq TRUE assign default value to all nDimStates

		set new_dim [dict get $confDict new_$dim]
		#puts "  - get default values"
		foreach state [my GetStatesWithDim $dim] {
		    if {$state ni $initStates} {
			my variable $state
			#puts "   $state [my GetDefaultValue $state $new_dim]"
			dict set confDict new_$state\
			    [my GetDefaultValue $state $new_dim]
		    }
		}
	    }
	}


	next
    }


    method SetDimFixed {qualifier dimension} {
	if {![dict exists $stateDimInfo $qualifier]} {
	    error "qualifier \"$qualifier\" is not valid."
	}
	if {$qualifier eq ""} {
	    dict set stateDimInfo $qualifier isFixed FALSE
	    dict set stateDimInfo $qualifier fixedDim ""
	} else {
	    dict set stateDimInfo $qualifier isFixed TRUE
	    dict set stateDimInfo $qualifier fixedDim $dimension
#	    ## Now reset all non-dominant qualifier-dim states
#	    puts "get states with dim [my GetStatesWithDim $qualifier]"
#	    set args {}
#	    foreach state [my GetStatesWithDim $qualifier] {
#		set vals [my GetDefaultValue $state $dimension]
#		puts "vals =$vals"
#		lappend agrs -$state
#	    }
	}
    }


    method GetDefaultValue {state length} {
	if {[info exist defaultValues($state)]} {
	    if {[llength $defaultValues($state)] eq 0} {
		return [lrepeat $length {}]
	    } else {
		return [lrepeat $length $defaultValues($state)]
	    }
	} else {
	    error "default value for state $state does not exist"
	}
    }

    method AddDefaultValue {state value} {
	array set defaultValues [list $state $value]
    }


    method ProcessWhich {arg_which dim} {
	my variable $dim

	# puts "Process which for dim \"$dim\" and arg $arg_which"

	## dim is the dimension name, e.g. n, p, ..
	set dim_num [set $dim];# the actual numerical value

	if {$dim_num eq 0} {
	    return {}
	    #error "-which can not be specified if n=0."
	}


	## use state names for which
	## e.g. -which selected or -which active etc...
	## TODO ckeck if boolean
	if {[llength $arg_which] eq 1 && $arg_which in [my GetStatesWithDim $dim]} {
	    my variable $arg_which
	    set arg_which [set $arg_which]

	} elseif {$arg_which eq "all"} {
	    return [::loon::listfns::lseq 0 [expr {$n-1}]]
	}



	set nWhich [llength $arg_which]
	if {$nWhich eq 0} {
	    set which {}
	} elseif {$nWhich eq 1} {

	    switch -- $arg_which {
		all {
		    set which [::loon::listfns::lseq 0 [expr {$dim_num - 1}]]
		}
		default {
		    ## Special Case ony 1 element
		    ## Is index or logical subsetting
		    if {[string  toupper\
			     [string index $arg_which 0]] in {T F}} {

			## logical indexing
			if {$arg_which} {
			    set which 0
			} else {
			    set which {}
			}
		    } else {
			## Index subsetting
			if {$arg_which < 0 || $arg_which >= $n} {
			    error "-which value $which is out of bound."
			}

			set which $arg_which
		    }
		}
	    }
	} else {

	    ## nWhich > 0

	    set isLogicalSubsetting FALSE

	    if {[::loon::listfns::isBoolean $arg_which]} {

		  if {$nWhich eq 2} {

	          if {![::loon::listfns::isNumeric $arg_which]} {
			    set isLogicalSubsetting TRUE
		      }
		  } else {
		     set isLogicalSubsetting TRUE
		  }
	    }

	    if {$isLogicalSubsetting} {
		##set tmp [string toupper [string index [lindex $arg_which 0] 0]]
		##if { $tmp eq T || $tmp eq F } {}

		## Logical Subsetting
		if {$nWhich ne $dim_num} {
		    error "logical indexing needs -which to have\
                               length $n, not $nWhich."
		}

		set which [::loon::listfns::which $arg_which]

	    } else {


		## Index Subsetting
		foreach w $arg_which {
		    if {![string is integer $w]} {
			error "bad value \"$w\" for -which."
		    }
		    if {$w < 0 || $w >= $dim_num} {
			error "element $w in -which is out of bound."
		    }
		}
		set which $arg_which
	    }
	}

	return $which
    }

}
