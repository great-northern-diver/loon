

::oo::class create ::loon::classes::Navigator {
    
    superclass ::loon::classes::withContexts\
	::loon::classes::withStateBindings

    variable graph navigatorId graphStateBinding from to proportion label\
	nodes_var from_var to_var widgetSubst inAnimation

    constructor {Graph NavigatorId} {
	
	set graph $Graph
	set navigatorId $NavigatorId
	
	set widgetSubst "%nav $NavigatorId" ;# need these to add %W substitution to context

	set graphStateBinding [$graph systembind state add all "[self namespace]::my GraphUpdate %e"]

	set from [lindex [$graph cget -nodes] 0]
	set to ""
	set proportion 0

	set inAnimation FALSE
	
	set graphns [info object namespace $graph]
	foreach state {nodes from to} {
	    set ${state}_var [uplevel #0 [list ${graphns}::my varname $state]]
	}
	
	next
	
	my New_state tag string any ""
	
	my New_state color color 1 "#fdfd96"
	
	my New_state animationPause positive_integer 1 5
	
	my New_state animationProportionIncrement positive_double 1 0.008
		
	my New_state scrollProportionIncrement positive_double 1 0.02

	my AddStates from to proportion

	my New_state label string any ""
	
	my SetStateInfo proportion in_unit_interval 1 0




	my SetStateDescription tag\
	    "tags useful for item bindings"

	my SetStateDescription color\
	    "fill color of navigator"
	
	my SetStateDescription animationPause\
	    "delay in ms before moving the navigator to the next position"

	my SetStateDescription animationProportionIncrement\
	    "proportion increment in animation"
	
	my SetStateDescription scrollProportionIncrement\
	    "proportion increment when moving the navigator with the mouse scroll wheel"

	my SetStateDescription from\
	    "list of graph nodes. The last -from element, the first -to element and the -proportion state determine the location of the navigator."
	
	my SetStateDescription to\
	    "empty list or list of graph nodes. If empty, then there is no next node on the path. The last -from element, the first -to element and the -proportion state determine the location of the navigator."
		
	my SetStateDescription proportion\
	    "value between 0 and 1. The last -from element and the first -to element determine the location of the navigator."

	my SetStateDescription label\
	    "text label on navigator"
	
    }


    destructor {
	$graph systembind state delete $graphStateBinding
    }

    method InfoDebug {args} {
	next graph from to proportion {*}$args
    }
    
    
    method EvalConfigure {} {
	my variable confDict

	## Need initialization

	set hasFrom [dict get $confDict has_from]
	set hasTo [dict get $confDict has_to]

	## are from nodes valid nodes
	if {$hasFrom} {
	    set nfrom [dict get $confDict arg_from]
	    my CheckIfValidNodes $nfrom
	} else {
	    set nfrom $from
	}
	
	## are to nodes valid nodes
	if {$hasTo} {
	    set nto [dict get $confDict arg_to]
	    my CheckIfValidNodes $nto
	} else {
	    set nto $to
	}

	
	if {$nfrom eq "" && $nto ne ""} {
	    set hasFrom TRUE
	    set hasTo
	    set nfrom $nto
	    set nto ""
	}
	
	if {$hasFrom || $hasTo} {
	    my CheckIfValidPath [concat $nfrom $nto]
	}
	
	if {$hasFrom} {
	    dict set confDict new_from $nfrom
	}
	if {$hasTo} {
	    dict set confDict new_to $nto
	}
	
	if {[dict get $confDict has_proportion]} {
	    set arg_p [dict get $confDict arg_proportion]
	    
	    if {![string is double $arg_p]} {
		error "-proportion is not numeric."
	    }
	    if {$arg_p < 0 || $arg_p > 1} {
		error "-proportion must be a number between 0 and 1."
	    }
	    dict set confDict new_proportion $arg_p
	}

	next
    }

    ## empty string is also valid for from and to
    method CheckIfValidNodes {nodes} {
	set cur_nodes [set $nodes_var]
	foreach node $nodes {
	    if {$node ni $cur_nodes} {
		error "node \"$node\" does not exist."
	    }
	}
	return TRUE
    }
    
    method CheckIfValidPath {path} {
	set ifrom ""
	    foreach ito $path {
		if {[llength $ifrom] > 0} {
		    if {![$graph hasEdge $ifrom $ito]} {
			error "edge $ifrom - $ito does not exist"
		    }
		    if {![$graph isEdgeSeen $ifrom $ito]} {
			error "edge $ifrom - $ito is not seen (adjoining nodes or edge is not active)."
		    }
		}
		set ifrom $ito
	    }
	return TRUE
    }
    
    method GraphUpdate {events} {

	#puts "Navigator graph $graph has updated: $events"
	
	if {$events eq "selected"} {return}
	
	if {"nodes" in $events ||\
		"active" in $events ||\
		"from" in $events ||\
		"to" in $events ||\
		"activeEdge" in $events} {

	    if {$from eq ""} {
		my configure -from [lindex [$graph cget -nodes] 0] -to "" -proportion 0
		return
	    }
	    
	    set path [concat $from $to]
	    if {[catch {my CheckIfValidNodes $path}] || \
		    [catch {my CheckIfValidPath $path}] } {
		## path is not value, need to reset from and to

		## move navigator to the first visible node
		set foundVisibleNode FALSE
		foreach node [set $nodes_var] {
		    if {[uplevel #0 [list $graph isNodeSeen $node]]} {
			my configure -from $node -to "" -proportion 0
			set foundVisibleNode TRUE
			break
		    }
		}
		if {!$foundVisibleNode} {
		    my configure -from "" -to "" -proportion 0
		}
	    }
	}
    }

    method walk {what args} {
	## Cancel current animation
	set inAnimation FALSE
	
	switch -- $what {
	    backward {
		set inReverse TRUE

		switch -- [llength $args] {
		    0 {
			set nedgestogo [llength $from]
			incr nedgestogo -1
		    }
		    1 {
			set i [lsearch -exact $from $args]
			if {$i >= 0} {
			    set nedgestogo [expr {[llength $from] - $i - 1}]
			} else {
			    error "could not find node \"$args\" in -from state."
			}
		    }
		    default {
			error "walk forward: wrong to node: \"$args\""
		    }
		}

	    }
	    forward {
		set inReverse FALSE

		switch -- [llength $args] {
		    0 {
			set nedgestogo [llength $to]
			incr nedgestogo -1			
		    }
		    1 {
			set i [lindex [lsearch -exact -all $to $args] end]
			if {$i >= 0} {
			    set nedgestogo $i
			} else {
			    error "could not find node \"$args\" in -to state"
			}
		    }
		    default {
			error "walk forward: wrong to node: \"$args\""
		    }
		}
		
	    }
	    path {
		set path {*}$args

		my configure -from [lindex $path 0] -to [lrange $path 1 end]\
		    -proportion 0
		update
#		after $animationPause		
		
		set inReverse FALSE		
		set nedgestogo [llength $to]
		incr nedgestogo -1
	    }
	    default {
		error "walk \"$what\" is not valid. Use forward, backward, or path" 
	    }
	}

	after idle [list [self namespace]::my WalkNow $inReverse $nedgestogo]
	return
    }


    method WalkNow {inReverse nedgestogo} {
	my variable animationPause animationProportionIncrement
	
	set inAnimation TRUE
	set pincr $animationProportionIncrement
	set p $proportion
		
	if {$inReverse} {
	    
	    while {$inAnimation && $nedgestogo >= 0} {
		
		set p [expr {$p-$pincr}]		
		if {$p < 0} {
		    
		    if {[llength $from] eq 1} {
			set nedgestogo -1
			set p 0
			my configure -proportion $p
		    } else {
			set p 1
			
			my configure\
			    -proportion $p\
			    -from [lrange $from 0 end-1]\
			    -to [concat [lindex $from end] $to]
		    }
		    incr nedgestogo -1
		} else {
		    my configure -proportion $p
		}
		
		uplevel #0 update
		after $animationPause
	    }
	} else {
	    
	    while {$inAnimation && $nedgestogo >= 0} {
		set p [expr {$p+$pincr}]

		if {$p > 1} {
		    set p 0
		    
		    my configure\
			-proportion $p\
			-from [concat $from [lindex $to 0]]\
			-to [lrange $to 1 end]
		    
		    incr nedgestogo -1
		} else {
		    my configure -proportion $p
		}
		uplevel #0 update
		after $animationPause
	    }
	}
	set inAnimation FALSE	
    }

    method stopAnimation {} {
	set inAnimation FALSE
    }

    method inAnimation {} {
	return $inAnimation
    }
    
    method addToPath {node} {
	my configure -to [concat $to $node]
    }

    method SetSubstitutions {subst} {
	set widgetSubst $subst
	next $subst
    }

    method AddSubstitutions {subst} {
	lappend widgetSubst {*}$subst
	next $subst
    }
    
}
    
