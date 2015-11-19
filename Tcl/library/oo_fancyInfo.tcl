

oo::class create ::loon::classes::fancyInfo {
   
    ## the constructor and destructor also work as method chain sink
    constructor {args} {

    }
    
    destructor {
    }

    method SetSubstitutions {subst} {
	
    }

    method AddSubstitutions {subst} {
	
    }

    method setModel {args} {}

    method init {} {}
    
    method info {args} {

	if {[llength $args] eq 0} {
	    ## TODO make a general info output
	    puts "Object of class [info object class [self]]"
	    puts "info on states run: info states"

	} else {
	    set subcommand [lindex $args 0]
	    set args [lreplace $args 0 0]

	    ## stateDimension and stateType need to be fast
	    switch -- $subcommand {
		stateDimension {
		    set state $args
		    my variable managedStates stateInfo
		    if {[dict exists $managedStates $state]} {
			set sdim [dict get $managedStates $state dim]
		    } elseif {[dict exists $stateInfo $state]} {
			set sdim [dict get $stateInfo $state dim]
		    } else {
			set sdim -1
		    }		    
		    return $sdim
		}
		stateType {
		    set state $args
		    my variable managedStates stateInfo		    
		    if {[dict exists $managedStates $state]} {
			set stype [dict get $managedStates $state type]
		    } elseif {[dict exists $stateInfo $state]} {
			set stype [dict get $stateInfo $state type]
		    } else {
			set stype ?
		    }
		    return $stype
		}
		states - 
		state {
		    my InfoStates {*}$args
		}
		debug {
		    my InfoStates {*}$args
		}
		default {
		    ## or debug
		    my InfoDebug $subcommand {*}$args
		}
	    }
	}
	
    }

    method InfoStates {args} {
	
	my variable configurableOptions

	switch -- [llength $args] {
	    0 {
		## display all states
		set args $configurableOptions
	    }
	    1 {
		set args {*}$args
	    }
	}
	
	foreach state $args {
	    if {$state ni $configurableOptions} {
		error "\"$state\" is not a valid state."
	    }
	}
	
	## return all information about a state
	my variable managedStates stateDescriptions stateInfo
	
	set out [dict create]
	foreach state $args {
	    
	    if {[dict exists $managedStates $state]} {
		set stype [dict get $managedStates $state type]
		set sdim [dict get $managedStates $state dim]
		set sdefault [dict get $managedStates $state init]
	    } elseif {[dict exists $stateInfo $state]} {
		set stype [dict get $stateInfo $state type]
		set sdim [dict get $stateInfo $state dim]
		set sdefault [dict get $stateInfo $state init]
	    } else {
		set stype ?
		set sdim -1
		set sdefault ? 
	    }
	    
	    if {[dict exists $stateDescriptions $state]} {
		set sdescription [dict get $stateDescriptions $state]
	    } else {
		set sdescription ?
	    }
	    
	    dict set out $state\
		[dict create\
		     type $stype\
		     dimension $sdim\
		     defaultvalue $sdefault\
		     description $sdescription]
	}
	#if {[dict size $out] eq 1} {
	#    set state [lindex [dict keys $out] 0]
	#    set out [dict get $out $state]
	#}
	
	return $out
    }
    
    method InfoDebug {args} {
	
	puts "Object of class [info object class [self]]"
	
	set variables $args
	
	set maxLength 0
	foreach var $variables {
	    set ns [string length $var]
	    if {$ns>$maxLength} {
		set maxLength $ns
	    }
	}
	
	incr maxLength 2
	
	foreach var $variables {
	    my variable $var

	    if {[array exists $var]} {
		puts [format "%${maxLength}s: -- array" $var]
		my Parray [array get $var] [expr {$maxLength+2} ]
	    } elseif {[info object isa object [set $var]]} {
		puts [format "%${maxLength}s: %s" $var [set $var]]		
	
#		if {[info object isa\
#			 typeof [set $var]\
#			 ::loon::classes::labelledObjectsTree]} {
#		    [set $var] printTree root [string repeat " " $maxLength]
#		    puts [format "%${maxLength}s  ---" ""]
#		} 
		
	    } else {
		puts [format "%${maxLength}s: %s" $var [set $var]]
	    }
	}
    } 

    
    method Parray {flatArray {nindent 0}} {

	array set arr $flatArray
	
	set maxLength 0
	foreach var [array names arr] {
	    set ns [string length $var]
	    if {$ns>$maxLength} {
		set maxLength $ns
	    }
	}
	
	if {$maxLength > $nindent} {
	    set nindent $maxLength
	}
	
	foreach var [array names arr] {
	    puts [format "%${nindent}s - %s" "$var" $arr($var)]
	}
	
    }
    
}
