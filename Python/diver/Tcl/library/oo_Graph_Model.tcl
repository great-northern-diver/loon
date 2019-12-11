
oo::class create ::loon::classes::Graph_Model {


    ## withNavigators needs to be bevor XYPair_Mode
    ## because otherwise it does not get put into the
    ## configure loop
    superclass 	::loon::classes::withNavigators\
	::loon::classes::XYPair_Model\
	::loon::classes::withLayers\

    
    variable nodeDict has_nodeDict new_nodeDict\
	edgeDict has_edgeDict new_edgeDict\
	adjacentDict has_adjacentDict new_adjacentDict\
	showEdge has_showEdge new_showEdge\
	iftedges new_iftedges is_newGraph
    
    
    constructor {args} {
	
	## the edge dict has key <from><to> if edge exists and the
	## value is whether the edge is seen
	set edgeDict [dict create];     # to check if an edge exists fast
	set adjacentDict [dict create]; #
	set nodeDict [dict create]
	
	set has_nodeDict FALSE
	set new_nodeDict ""
	set has_edgeDict FALSE
	set new_edgeDict ""
	set has_adjacentDict FALSE
	set new_adjacentDict ""
  
	set showEdge "" ; # boolean vector whether edge should be shown
	set has_showEdge FALSE
	set new_showEdge ""
	set iftedges "" 
	set new_iftedges ""
	set is_newGraph FALSE

	
	## initialize Scatterplot
	next {*}$args

	my AddDefaultValue size 28
	
	my New_state nodes string n ""
	
	my SetInitStates n nodes
	
	
	## edge states
	my New_state from string p ""
	my New_state to string p ""
	
	my SetInitStates p {from to}
	
	#my New_state edgeLabel    string p "edge"
	my New_state isDirected   boolean 1 FALSE

	my New_state activeEdge   boolean p TRUE
	my New_state colorEdge    color p black
	#my New_state selectedEdge boolean p FALSE
	
	
	## Orbital Labelled
	my New_state orbitDistance positive_double 1 15
	my New_state orbitAngle double n 0
	my New_state showOrbit boolean 1 TRUE
	


	my SetStateDescription nodes\
	    "graph node names"
	my SetStateDescription from\
	    "specify edges with from-to node pairs. This is a list of from nodes"
	my SetStateDescription to\
	    "specify edges with from-to node pairs. This is a list of to nodes"
	my SetStateDescription isDirected\
	    "boolean to specify whether the edges specified with 'from' and 'to' are directed or not"
	my SetStateDescription activeEdge\
	    "boolean to specify whether an edge is active or not"
	my SetStateDescription colorEdge\
	    "color of the edges"
	my SetStateDescription orbitDistance\
	    "distances of the orbiting node names"
	my SetStateDescription orbitAngle\
	    "angles of orbiting node names"
	my SetStateDescription showOrbit\
	    "show node names"
	
	
	my AddLayer model "graph"\
	    [::loon::classes::GraphLayer new [self]] root 0 "Graph"


	my SetStateDescription color "colors of the point glyphs"
	
    }

    method InfoDebug {args} {
	next {*}$args
    }
    
    method GetDefaultValue {state length} {

	## Circle Layout is the default x and y coordinates
	switch -- $state  {
	    x {
		set xc {}
		if {$length > 0} { 
		    for {set i 0} {$i < $length} {incr i} {
			set angle [expr {2*$::loon::pi*(1-double($i)/$length)}]
			lappend xc [expr {double(cos($angle))}]
		    }
		}
		return $xc
	    }
	    y {
		set yc {}
		if {$length > 0} { 
		    for {set i 0} {$i < $length} {incr i} {
			set angle [expr {2*$::loon::pi*(1-double($i)/$length)}]
			lappend yc [expr {-double(sin($angle))}]
		    }
		}
		return $yc
	    } 
	    orbitAngle {
		set angles {}
		if {$length > 0} { 
		    for {set i 0} {$i < $length} {incr i} {
			lappend angles [expr {2*$::loon::pi*double($i)/$length}]
		    }
		}
		return $angles
	    }
	    default {
		return [next $state $length]
	    }
	}
    }
    
    ## Check if edges are valid, i.e. they connect nodes
    ## State Rules: 
    ##   - only edges can be active if their nodes are active
    method EvalConfigure {} {
	my variable confDict

	next 


	## New graph ?
	set is_newGraph FALSE; # possible new graph

	set hasNew [lmap s {nodes from to} {dict exists $confDict new_$s}]	
	if {[expr [join $hasNew "||"]]} {
	    set is_newGraph TRUE
	    
	    foreach state {nodes from to} has $hasNew {
		if {$has} {
		    set cur_$state [dict get $confDict new_$state]
		} else {
		    my variable $state
		    set cur_$state [set $state]
		}
	    }

	    set new_iftedges {}
	    foreach f $cur_from t $cur_to {
		if {$f ni $cur_nodes} {
		    error "-from element \"$f\" is not a valid node."
		} else {
		    set i [lsearch -exact $cur_nodes $f]
		}
		if {$t ni $cur_nodes} {
		    error "-to element \"$t\" is not a valid node."
		} else {
		    set j [lsearch -exact $cur_nodes $t]
		}
		lappend new_iftedges $i $j		
	    }
	} 

	

	## TODO: State Rules? Needed?
	##  -active and -selected
	##  -activeEdge and -selectedEdge


	set has_showEdge FALSE; # possible change in which edges should be shown
	set changedActive [lmap s {active activeEdge} {dict exists $confDict new_$s}]
	if {[expr [join $changedActive "||"]] || $is_newGraph} {
	    set has_showEdge TRUE	  
	    foreach state {active activeEdge} has $changedActive {
		if {$has} {
		    set cur_$state [dict get $confDict new_$state]
		} else {
		    my variable $state
		    set cur_$state [set $state]
		}
	    }
	    
	    if {$is_newGraph} {
		set cur_iftedges $new_iftedges
	    } else {
		set cur_iftedges $iftedges
	    }

	    set new_showEdge {}
	    foreach {i j} $cur_iftedges a $cur_activeEdge {
		if {[lindex $cur_active $i] && [lindex $cur_active $j] && $a} {
		    lappend new_showEdge TRUE
		} else {
		    lappend new_showEdge FALSE
		}
	    }
	}
	
	## Always update dict if edge is shown
	if {$has_showEdge || [dict exists $confDict new_isDirected]} {
	    
	    if {$has_showEdge} {
		set cur_showEdge $new_showEdge
	    } else {
		set cur_showEdge $showEdge
	    }
	    
	    if {!$is_newGraph} {
		foreach state {nodes from to} {
		    my variable $state
		    set cur_$state [set $state]
		}
	    }
	    
	    if {[dict exists $confDict new_isDirected]} {
		set cur_isDirected [dict get $confDict new_isDirected]
	    } else {
		my variable isDirected
		set cur_isDirected $isDirected
	    }
	    
	    set new_edgeDict [dict create]
	    foreach f $cur_from t $cur_to s $cur_showEdge {
		dict set new_edgeDict <${f}><${t}> $s
		if {!$cur_isDirected} {
		    dict set new_edgeDict <${t}><${f}> $s
		} 
	    }
	    
	    set new_adjacentDict [dict create]
	    foreach f $cur_from t $cur_to {
		dict lappend new_adjacentDict $f $t
		if {!$cur_isDirected} {
		    dict lappend new_adjacentDict $t $f
		}
	    }
	    foreach node $cur_nodes {
		if {![dict exists $new_adjacentDict $node]} {
		    dict set new_adjacentDict $node {}
		}
	    }
	    
	    ## Doing the nodes here results in re-creating the node
	    ## dict too often but less code
	    
	    set new_nodeDict [dict create]
	    foreach node $cur_nodes a $cur_active {
		dict set new_nodeDict $node $a
	    }
	    
	    set has_nodeDict TRUE
	    set has_edgeDict TRUE
	    set has_adjacentDict TRUE

	} else {
	    set has_nodeDict FALSE
	    set has_edgeDict FALSE
	    set has_adjacentDict FALSE
	}
	
    }
    
    method HookAfterStatesSet {} {
	my variable changedStates

	if {$has_showEdge} { set showEdge $new_showEdge }
	if {$is_newGraph} { set iftedges $new_iftedges }
	if {$has_nodeDict} { set nodeDict $new_nodeDict }
	if {$has_edgeDict} { set edgeDict $new_edgeDict	}
	if {$has_adjacentDict} { set adjacentDict $new_adjacentDict }
	
	next 
	
    }
    
    
    method hasEdge {From To} {
	if {[dict exists $edgeDict <$From><$To>]} {
	    return TRUE
	} else {
	    return FALSE
	}
    }
    
    method isEdgeSeen {From To} {
	if {[dict exists $edgeDict <$From><$To>]} {
	    return [dict get $edgeDict <$From><$To>]
	} else {
	    return FALSE
	}
    }

    method hasNode {Node} {
	if {[dict exists $nodeDict $Node]} {
	    return TRUE
	} else {
	    return FALSE
	}	
    }
    
    method isNodeSeen {Node} {
	if {[dict exists $nodeDict $Node]} {
	    return [dict get $nodeDict $Node]
	} else {
	    return FALSE
	}	
    }
    
    method adjacent {Node} {
	if {[dict exists $adjacentDict $Node]} {
	    return [dict get $adjacentDict $Node]
	} else {
	    error "Node \"$Node\" does not exist"
	}
    }


    ## add more padding for graphs
    method ApplyScaltoChanges {what bbox {xyRangeFactor 1.2}} {

	if {$what in {plot world model}} {
	    next $what $bbox 1.7
	} else {
	    next $what $bbox
	}
	
    }
    
}
