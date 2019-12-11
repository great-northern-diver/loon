
oo::class create loon::classes::Graphswitch {

    superclass ::loon::classes::Inspector2
    
    variable treeview graph_dict nextid applyToActiveGraph
    
    constructor {Path} {

	set nextid 0
	set graph_dict [dict create]
	set applyToActiveGraph TRUE

	next $Path

    }
    
    
    method CheckNewActivewidget {widget} {
	if {![info object isa typeof $widget\
		  "::loon::classes::Graph_Widget"]} {
	    error "$widget is not a Graph Widget."
	}
    }
    
    method RegisterActivewidget {} {
	## Todo
    }
    
    
    method ActivewidgetEvents {args} {
	## Destroy events
    }
    
    
    method Make {} {
	my variable path
	
	frame $path -class LoonGraphswitch
	
	set treeview [::ttk::treeview ${path}.treeview\
			  -selectmode browse\
			  -columns id]
	
	$treeview heading #0 -text label -anchor c
	$treeview heading id -text id
	$treeview column id -width 100 -stretch FALSE
	$treeview column #0 -width 150 -stretch TRUE
	
	pack $treeview -side left -fill both -expand TRUE
	
	set scroll ${path}.scroll
	
	pack [::tk::scrollbar $scroll\
		  -orient vertical -command "$treeview yview"]\
	    -side left -anchor n -fill y
	
	$treeview configure -yscrollcommand "$scroll set"
	
	

	bind $treeview <<TreeviewSelect>> "[self namespace]::my TreeviewSelectEvent"
	
    }


    method AddExternal {nodes from to isDirected {label ""} {index end}} {
	my add [list $nodes $from $to $isDirected] $label $index
    }
    
    method add {graph {label ""} {index end}} {

	## check if graph is valid
	switch -- [llength $graph] {
	    3 {
		## append isDirected
		lappend graph FALSE
	    }
	    4 {
		## do nothing
	    }
	    default {
		error "graph must be a nested list with\
                       4 elements: nodes, from, to, and isDirected."
	    }
	}
	
	set id "graph$nextid"
	
	$treeview insert "" $index -id $id -text $label -values $id
	if {[my TVcurrentId] eq ""} {
	    my TVset $id FALSE
	}
	
	incr nextid
	
	dict set graph_dict $id $graph
	
	return $id	
    }
    
    
    method GetIds {} {
	return [$treeview children ""]
    }

    method ids {} {
	return [my GetIds]
    }
    
    method ErrorIfIdDoesntExist {id} {
	if {![$treeview exist $id]} {
	    error "id \"$id\" does not exist. Query all\
                   graph ids with the \"ids\" method."
	} 
    }

    method ErrorIfNoActivewidget {} {
	my variable activewidget
	if {$activewidget eq ""} {
	    error "No activewidget set."
	}
    }

    method relabel {id label} {
	
	$treeview item $id -text $label
	
	return $id
    }

    method getLabel {id} {
	my ErrorIfIdDoesntExist $id
	
	return 	[$treeview item $id -text]
    }
    
    
    method move {id index} {
	my ErrorIfIdDoesntExist $id
	
	$treeview move $id "" $index

	return [my GetIds]
    }
    
    method reorder {ids} {
	
	set graph_ids [my GetIds]
	set missing [::loon::listfns::setDiff $graph_ids $ids]
	
	if {[llength $missing]>0} {
	    error "Not all ids in argument list. The missing ids are\
                   \"[join $missing ,]\".\
                   Use the \"ids\" method to get a list of all ids."
	} 
	
	set tooMuch [::loon::listfns::setDiff $ids $graph_ids]
	if {[llength $missing]>0} {
	    error "Ids that do not exist are in the argument list.\
                   The ids that do\
                   not exist are \"[join $tooMuch ,]\".\
                   Use the \"ids\"  method to get a list of all ids."
	} 
	
	foreach id $ids {
	    $treeview move $id "" end
	}	
	
	return [my GetIds]
    } 

    method delete {id} {
	my ErrorIfIdDoesntExist $id
	
	if {[my TVcurrentId] eq $id} {
	    my TVset [$treeview next $id] FALSE
	}
	
	$treeview delete $id
	dict unset graph_dict $id

	return [my GetIds]
    }
    
    method get {id} {
	my ErrorIfIdDoesntExist $id

	return [dict get $graph_dict $id]
    }


    method set {id} {
	my TreeviewToActivewidget $id
	my TVset $id
    }
    

    ## return id of currently selected graph
    method TVcurrentId {} {
	
	set id [$treeview selection]
	
	return $id
    }
    
    method ErrorOrCurrentId {} {
	set id [my TVcurrentId]
	
	if {$id eq ""} {
	    error "no graph selected."    
	}
	
	return $id
    }
    
    
    ## set the selection to id
    method TVset {id {changeGraphToActiveWidget TRUE}} {
	
	my ErrorIfIdDoesntExist $id
	
	set applyToActiveGraph $changeGraphToActiveWidget
	
#	puts "apply to active graph: $applyToActiveGraph "

	$treeview selection set $id
	update
	
	set applyToActiveGraph TRUE

    }
    
    
    ## Button Update
    method ButtonUpdate {} {
	set id [my ErrorOrCurrentId]
	my ActivewidgetToTreeview $id
    }

    ## Button set
    method ButtonSet {} {
	set id [my ErrorOrCurrentId]
	my TreeviewToActivewidget $id
    }
    
    ## Select Gesture on Treeview
    method TreeviewSelectEvent {} {
	
	if {$applyToActiveGraph} {
	    set id [my ErrorOrCurrentId]
	    my TreeviewToActivewidget $id
	}
	
    }
    
    
    method TreeviewToActivewidget {id} {
	my variable activewidget
	
	my ErrorIfNoActivewidget
	my ErrorIfIdDoesntExist $id
	
	set G [dict get $graph_dict $id]
	
	uplevel #0 [list $activewidget configure\
			-nodes [lindex $G 0]\
			-from [lindex $G 1]\
			-to [lindex $G 2]\
			-isDirected [lindex $G 3]]
	
	uplevel #0 [list $activewidget scaleto world]
    }

    method ActivewidgetToTreeview {id} {
	my variable activewidget
	
	my ErrorIfNoActivewidget
	my ErrorIfIdDoesntExist $id
	
	dict set graph_dict $id\
	    [list [$activewidget cget -nodes]\
		 [$activewidget cget -from]\
		 [$activewidget cget -to]\
		 [$activewidget cget -isDirected]]	    
    }
    
}

