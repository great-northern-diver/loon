
::oo::class create ::loon::classes::Linkable {

    superclass ::loon::classes::VariableDimensions
    
    variable linkingGroup linkingKey notLinkableStates\
	isDefaultLinkingKey\
	linkedStates syncArg doNotSyncArg

    constructor {args} {
	
	set linkingGroup none
	set linkingKey {}
	
	set notLinkableStates {}
	set linkedStates {}
	set syncArg ""           ;# -sync push/pull
	set doNotSyncArg ""      ;# .noSync TRUE
	set isDefaultLinkingKey TRUE ;# this is used to speed up linking

	::loon::registerForLinking [self namespace] none
	
	next {*}$args

	my AddStates linkingGroup linkingKey
	my AddStatesToVariableDimension linkingKey n
	my AddNotLinkableStates linkingKey

	my SetStateInfo linkingGroup string 1 "none"
	my SetStateInfo linkingTag string n "{point0 point1 ...}"
	
    }

    method InfoDebug {args} {
	next linkingGroup linkingKey isDefaultLinkingKey linkedStates {*}$args
    }
    
    method AddNotLinkableStates {states} {
	lappend notLinkableStates {*}$states
    }
    
    method setLinkedStates {states} {

	set linkableStates [::loon::listfns::setDiff\
				[my GetStatesWithDim n]\
				$notLinkableStates]
	
	foreach state $states {
	    if {[string range $state 0 0] eq "-"} {
		set state [string replace $state 0 0]
	    }
	    if {$state ni $linkableStates} {
		error [format "state \"%s\" is not an n-dimensional state and\
                               hence can not be linkable. Linkable states are: %s."\
			   $state [join $linkableStates ", "]]
	    }
	}
	set linkedStates $states
    }
    
    method getLinkedStates {} {
	return $linkedStates
    }
    
    method GetDefaultValue {state length} {
	if {$state eq "linkingKey"} {
	    if {$length eq 0} {
		return {}
	    } else {
		incr length -1
		return [::loon::listfns::lseq 0 $length]
	    }
	} else {
	    return [next $state $length]
	}
   }

    method configure {args} {
	
#	set out {}
#	foreach {var val} $args {
#	    lappend out $var
#	    if {$var in {-which}} {
#		lappend out $val
#	    }
#	}
#	puts "-> [self], args: $out"
	
	## look for -sync
	
	## remove arguments used for linking
	set syncArg [::loon::listfns::extractAndRemoveOption args -sync ""]
	set doNotSyncArg [::loon::listfns::extractAndRemoveOption args .nosync FALSE]

	next {*}$args
	
    }

    method EvalConfigureInit {} {
	my variable confDict
	
	next 
	
	## Check -linkingGroup
	set has_linkingGroup [dict get $confDict has_linkingGroup]
	if {$has_linkingGroup} {
	    set arg_linkingGroup [dict get $confDict arg_linkingGroup]
	    if {$linkingGroup eq $arg_linkingGroup} {
		set has_linkingGroup FALSE
	    } else {
		if {$arg_linkingGroup eq ""} {
		    error "-linkingGroup must be a non-empty string."
		}
	    }
	}
	

	
	## Check -linkingKey
	## n after configure
	set new_n [my GetConfigureDim n]
	set has_linkingKey [dict get $confDict has_linkingKey]
	if {$has_linkingKey} {
	    
	    set arg_linkingKey [dict get $confDict arg_linkingKey]
	    if {$arg_linkingKey eq $linkingKey} {
		set has_linkingKey FALSE		
	    } else {
		set n_linkingKey [llength $arg_linkingKey]
		if {$n_linkingKey ne $new_n} {
		    error "-linkingKey has length $n_linkingKey but length $new_n is expected."
		}
		set slinkingKey [lsort -unique $arg_linkingKey]
		if {[llength $slinkingKey] ne $new_n} {
		    error "The elements in -linkingKey are not unique."
		}
	    }
	}
	
	if {$has_linkingGroup} {
	    dict set confDict new_linkingGroup $arg_linkingGroup	    
	}
	if {$has_linkingKey} {
	    dict set confDict new_linkingKey $arg_linkingKey
	}
	
	
	## Synchronize pull if necessary
	## Synchronize push can always be done at the of the configure routine
	
	

	if {!$has_linkingGroup} {
	    set arg_linkingGroup $linkingGroup
	}
	
	if {[::loon::hasGroupLinkedMembers $arg_linkingGroup [self namespace]] && $new_n > 0} {

	    set needSync TRUE
	    
	    if {[dict exists $confDict init_n] && [dict get $confDict init_n]} { 
		set hasInitN TRUE
	    } else {
		set hasInitN FALSE
	    }
	    
	    if {$hasInitN} {
		
		if {$syncArg ne ""} {
		#    puts "Warning: -sync has no effect when re-initializing\
                #                         n-dimensional states.\
                #                        -sync pull is always used."
		}
		set syncArg pull
		
	    } elseif {$has_linkingGroup || $has_linkingKey} {
		
		##- Has linkingKey or linkingGroup"
		##   -sync push pull needed"
		##       > pull into confDict"
		##       > push can wait until later"
		
		if {$syncArg ne "push" && $syncArg ne "pull"} {
		    error "Changing the -linkingGroup or -linkingKey when the display is\
                           linked requires the -sync argument to be either push or pull."
		}
		
	    } else {
		set needSync FALSE
	    }
	    
	    
	    if {$needSync && $syncArg eq "pull"} {

		## make sure that the most up-to date linkingKey and
		## linkingGroup are used
		if {!$has_linkingKey} {
		    if {$hasInitN} {
			set arg_linkingKey [my GetDefaultValue linkingKey $new_n]
		    } else {
			set arg_linkingKey $linkingKey		
		    }
		}
		
		## the dict entry gets initialized with the current state.
		set initState [dict create]
		if {$hasInitN} {
		    ## use newly initialized states from VariableDimensions
		    foreach state $linkedStates {
			dict set initState $state [dict get $confDict new_$state]
		    }
		} else {
		    ## use existing states
		    foreach state $linkedStates {
			my variable $state
			dict set initState $state [set $state]
		    }		
		}
		
		set pullStates [::loon::synchronizePull [self namespace]\
				    $arg_linkingGroup $arg_linkingKey\
				    $initState]
		
		#puts "--- sync pull [self namespace] with linkingKey: $linkingKey"
		dict for {state value} $pullStates {
		    dict set confDict new_$state $value
		}
		
	    }
	}	
    }
    
    method ApplyConfigure {} {
	my variable changedStates confDict

	next

	## By now no error should be called anymore
	
	## Is it a default linkingKey 0..(n-1)
	if {"linkingKey" in $changedStates} {
	    set isDefaultLinkingKey TRUE
	    set i 0
	    foreach l $linkingKey {
		if {$l ne $i} {
		    set isDefaultLinkingKey FALSE
		    break
		}
		incr i
	    }
	}

	
	if {"linkingGroup" in $changedStates} {
	    
	    ::loon::changeLinkingGroup [self namespace]
	
	    ## ApplyConfigure Post Linkable
	    ##  - change the linkingGroup in loon
	}
		
	if {[::loon::hasModelLinkedMembers [self namespace]] && !$doNotSyncArg} {
	    
	    if {"linkingKey" in $changedStates || "linkingGroup" in $changedStates} {
		set statesToPush $linkedStates
	    } else {
		set statesToPush {}
		foreach state $linkedStates {
		    if {$state in $changedStates && [dict get $confDict has_$state]} {
			lappend statesToPush $state
		    }
		}
	    }
	    
	    if {[llength $statesToPush] > 0} {
		## ApplyConfigure Post Linkable
		##   - linked states $statesToPush have changed: push
		::loon::synchronizePush [self namespace] $statesToPush
	    }
	}
    }

    destructor {
	::loon::removeFromLinking [self namespace]
	next
    }
        
}
