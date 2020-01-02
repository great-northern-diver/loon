
namespace eval loon {
    
    namespace export completegraph linegraph complement

    proc completegraph {nodes {isDirected FALSE}} {
	set from {}
	set to {}
	if {$isDirected} {
	    foreach nfrom $nodes {
		foreach nto $nodes {
		    if {$nfrom ne $nto} {
			lappend from $nfrom
			lappend to $nto
		    }
		}
	    }
	} else {
	    for {set i 0} {$i < [llength $nodes]} {incr i} {
		for {set j [expr {$i+1}]} {$j < [llength $nodes]} {incr j} {
		    lappend from [lindex $nodes $i]
		    lappend to [lindex $nodes $j]
		}
	    }
	}
	return [list $nodes $from $to $isDirected]
    }
    
    proc linegraph {nodes from to {isDirected FALSE} {separator ":"}} {
	
	if {[llength $from] ne [llength $to]} {
	    error "length of from and to differs"
	}

	set newfrom {}
	set newto {}
	
	if {$isDirected} {
	    foreach nfrom1 $from nto1 $to {
		foreach nfrom2 $from nto2 $to {
		    if {!($nfrom1 eq $nfrom2 && $nto1 eq $nto2)} {
			## Do they share a node?
			if {$nfrom1 eq $nfrom2 || $nfrom1 eq $nto2 ||\
				$nto1 eq $nfrom2 || $nto1 eq $nto2} {
			    lappend newfrom [format "%s%s%s"\
						 $nfrom1 $separator $nto1]
			    lappend newto [format "%s%s%s"\
					       $nfrom2 $separator $nto2]
			}
		    }
		}
	    }
	} else {
	    for {set i 0} {$i < [llength $from]} {incr i} {
		for {set j [expr {$i+1}]} {$j < [llength $from]} {incr j} {
		    set nfrom1 [lindex $from $i]
		    set nto1 [lindex $to $i]
		    set nfrom2 [lindex $from $j]
		    set nto2 [lindex $to $j]
		    if {!($nfrom1 eq $nfrom2 && $nto1 eq $nto2)} {
			## Do they share a node?
			if {$nfrom1 eq $nfrom2 || $nfrom1 eq $nto2 ||\
				$nto1 eq $nfrom2 || $nto1 eq $nto2} {
			    lappend newfrom [format "%s%s%s"\
						 $nfrom1 $separator $nto1]
			    lappend newto [format "%s%s%s"\
					       $nfrom2 $separator $nto2]
			}
		    }
		}
	    }
	}
	
	set nodes [lsort -unique [concat $newfrom $newto]]
	
	return [list $nodes $newfrom $newto $isDirected]
    }
    
    
    proc complement {nodes from to {isDirected FALSE}} {
	
	set ft [dict create]

	foreach f $from t $to {
	    dict set ft "<${f}><${t}>" 1
	    if {!$isDirected} {
		dict set ft "<${t}><${f}>" 1
	    }
	}
	
	set newfrom ""
	set newto ""

	if {$isDirected} {
	    foreach nfrom $nodes {
		foreach nto $nodes {
		    if {$nfrom ne $nto} {
			if {![dict exists $ft "<${nfrom}><${nto}>"]} {
			    lappend newfrom $nfrom
			    lappend newto $nto
			}
		    }
		}
	    }
	} else {
	    for {set i 0} {$i < [llength $nodes]} {incr i} {
		for {set j [expr {$i+1}]} {$j < [llength $nodes]} {incr j} {
		    if {![dict exists $ft\
			     [format "<%s><%s>" [lindex $nodes $i] [lindex $nodes $j]]]} {
			lappend newfrom [lindex $nodes $i]
			lappend newto  [lindex $nodes $j] 
		    }
		}
	    }
	}
	return [list $nodes $newfrom $newto $isDirected]
    }

    
    
}
