

::oo::class create ::loon::classes::Histogram_Model {
    
    superclass ::loon::classes::Plot_Model\
	::loon::classes::withLayers
    
    variable minX minY maxX maxY\
	bins new_bins has_bins\
	colorStackingOrder\
	binalgRun
	

    constructor {} {
	
	set bins [dict create bin [dict create] binid {} binalgRun 0]
	## binid ...  id  [dict create count [dict create all "" selected ""] x0 "" x1 ""]
	set new_bins ""
	set has_bins FALSE
	
	set colorStackingOrder selected
	
	set minX 0
	set minY 0
	set maxX 1
	set maxY 1

	set binalgRun 0 ;# every time the binning algorithm this gets
			 # increased
	
	next

	## Default y labels 
	
	my New_state default_ylabels string 3\
	    [list "Frequency" "Density" "Conditional Probability"]
	
	my variable ylabel
	set ylabel "Frequency"
	
	my New_state x double n ""
	
	my New_state binwidth positive_double 1 1
	my New_state origin double 1 0
	my New_state showBinHandle boolean 1 TRUE
	
	my New_state yshows factor 1 frequency {frequency density}

	my AddStates colorStackingOrder

	my New_state showOutlines boolean 1 TRUE
	my New_state showStackedColors boolean 1 FALSE
	my New_state colorFill color 1 thistle
	my New_state colorOutline color 1 black

	
	my New_state color color n steelblue
	my New_state selected boolean n FALSE
	my New_state active boolean n TRUE


	my New_state selectBy factor 1 sweeping {sweeping brushing} 
	my New_state selectionLogic factor 1 select {select deselect invert} 


	my SetInitStates n x
	my AddNotLinkableStates x
	my setLinkedStates {color selected active}

	my New_state useLoonInspector boolean 1 TRUE


	my AddLayer model "histogram"\
	    [::loon::classes::HistogramLayer new [self]] root 0 "Histogram"
	
    }
    
    method EvalConfigure {} {
	my variable confDict binwidth origin x ylabel
	
	


	next

	if {[dict get $confDict has_colorStackingOrder]} {
	    set new_cso {}
	    
	    foreach col [dict get $confDict arg_colorStackingOrder] {
		if {$col eq "selected"} {
		    lappend new_cso selected
		} else {
		    if {[::loon::listfns::isColor $col]} {
			lappend new_cso [::loon::listfns::toHexcolor $col]
		    } else {
			error "-colorStackingOrder: color \"$col\" is not a valid color."
		    }
		}
	    }
	    if {"selected" ni $new_cso} {
		set new_cso [linsert $new_cso 0 selected]
	    }
	    if {[llength $new_cso] ne [llength [lsort -unique $new_cso]]} {
		error "-stackingOrder: elements are not unique."
	    }
	    
	    dict set confDict new_colorStackingOrder $new_cso
	}

	## take care of y label
	if {[dict get $confDict has_yshows] &&\
		![dict get $confDict has_ylabel]} {
	    
	    if {[dict exists $confDict new_default_ylabels]} {
		set cur_default_ylabels [dict get $confDict new_default_ylabels]
	    } else {
		my variable default_ylabels
		set cur_default_ylabels $default_ylabels
	    }
	    
	    switch -- [dict get $confDict new_yshows] {
		frequency {
		    set new_ylabel [lindex $cur_default_ylabels 0]
		}
		density {
		    set new_ylabel [lindex $cur_default_ylabels 1]
		}		
		default {
		    my variable ylabel
		    set new_ylabel $ylabel
		}
	    }
	    dict set confDict new_ylabel $new_ylabel
	}


	set has_bins FALSE
	
	set hasX [dict exists $confDict new_x]
	set hasBinwidth [dict exist $confDict new_binwidth]
	set hasOrigin [dict exist $confDict new_origin]
	set hasActive [dict exist $confDict new_active]
	set hasSelected [dict exist $confDict new_selected]
	set hasColor [dict exist $confDict new_color]
	set hasYshows [dict exists $confDict new_yshows]
	
	
	## get default binwidth and origin
	if {$hasX} {
	    set mm [::loon::listfns::MinMax [dict get $confDict new_x]]
	    
	    if {!$hasBinwidth} {
		set tmpbw [expr {([lindex $mm 1] - [lindex $mm 0])/30.0}]
		if {$tmpbw < .0001} {
		    set tmpbw .00005
		}
		dict set confDict new_binwidth $tmpbw
		
		set hasBinwidth TRUE
	    }
	    
	    if {!$hasOrigin} {
		dict set confDict new_origin [lindex $mm 0]
		set hasOrigin TRUE
	    }
	    
	}
	

	if {$hasX || $hasBinwidth || $hasOrigin ||\
		$hasActive || $hasSelected || $hasColor ||\
		$hasYshows} {
	    
	    if {$hasX} {
		set cur_x [dict get $confDict new_x]
	    } else {
		set cur_x $x
		set mm [::loon::listfns::MinMax $cur_x]
	    }

	    foreach state {binwidth origin selected active color yshows} {
		if {[set "has[string toupper $state 0]"]} {
		    set cur_$state [dict get $confDict new_$state]
		} else {
		    my variable $state
		    set cur_$state [set $state]
		}
	    }

	    incr binalgRun
	    set new_bins [::loon::binningalg $cur_x $cur_binwidth $cur_origin\
			      $cur_active $cur_selected $cur_color\
			      $cur_yshows $binalgRun]
	    
	    
	    set has_bins TRUE
	    
	    
	    if {[llength $cur_x] > 0} {
		dict set confDict new_minX [lindex $mm 0]
		
		set max 0
		dict with new_bins {
		    dict for {id d} $bin {
			set c [dict get $d x1]
			if {$c > $max} {
			    set max $c
			}
		    }}
		dict set confDict new_maxX $max
		
		dict set confDict new_minY 0
		set max 0
		dict with new_bins {
		    dict for {id d} $bin {
			set c [dict get $d count all]
			if {$c > $max} {
			    set max $c
			}
		    }}
		dict set confDict new_maxY $max
	    } else {
		dict set confDict new_minX 0
		dict set confDict new_minY 0
		dict set confDict new_maxX 1
		dict set confDict new_maxY 1
	    }
	    
	    
	    
	}
	
    } 
    
    method HookAfterStatesSet {} {
	
	if {$has_bins} { set bins $new_bins}
	
	next
    }

    method InfoDebug {args} {
	next xorigins count frequency {*}$args
    }

    method binid2pointindecies {binid {color ""}} {
	if {![dict exists $bins $binid]} {
	    error "bin id \"$binid\" does not exist."
	}
	return [dict get $bins $binid points]
    }
    
    method pointindex2binid {index} {
	return [lindex [dict get $bins binid] $index]
    }
    
}


## origin can be any bin start
## used algorithm from
## http://stackoverflow.com/questions/2387916/looking-for-a-histogram-binning-algorithm-for-decimal-data
## as a starting point
proc ::loon::binningalg {xvals binwidth origin active\
			     selected color {binningtype frequency} {binalgRun ""}} {

    if {$binwidth <= 0} {
	error "bindiwth must be a positive number."
    }
    
    
    ## bins data structure
    ## 
    ## binid: n dimensional list with binid for each point
    ## 
    ##  ...,-1,0,1,... (binids):
    ##     count
    ##        all
    ##        selected
    ##        <color>
    ##     points
    ##        all
    ##        selected
    ##        <color>
    ##     x0
    ##     x1

    set bins [dict create bin [dict create] binid {}]
    
    if {[llength $binalgRun] > 0} {
	dict set bins binalgRun $binalgRun
    }

    set i 0
    foreach x $xvals a $active col $color sel $selected {
	if {$a} {
	    set binid [expr {int(floor(($x-$origin)/$binwidth))}]
	    if {![dict exists $bins bin $binid]} {
		dict set bins bin $binid\
		    [dict create count [dict create all 0 selected 0]\
			 points [dict create all {} selected {}]]
	    }
	    
	    dict with bins bin $binid {dict incr count all}
	    dict with bins bin $binid {dict lappend points all $i}
	    
	    if {$sel} {
		dict with bins bin $binid {dict incr count selected}
		dict with bins bin $binid {dict lappend points selected $i}
	    } else {
		dict with bins bin $binid {dict incr count $col}
		dict with bins bin $binid {dict lappend points $col $i}
	    }
	    
	} else {
	    set binid NA
	}
	
	incr i
	dict lappend bins binid $binid
    }
   
    ## now get x0 and x1 for each bin
    ## need to get both as otherwise i might get pixel errors
    dict with bins {dict for {id value} $bin {
    	dict set bin $id x0 [expr {$origin + $id * $binwidth}]
    	dict set bin $id x1 [expr {$origin + ($id + 1)* $binwidth}]
    }}
    
    
    switch -- $binningtype {
	density {
	    if {[::loon::listfns::any $active]} {
			
		set n 0
		foreach a $active {
		    if {$a} {incr n}
		}
		
		set area [expr {$binwidth * $n}]
		
		## divide by area
		dict with bins {
		    dict for {id value} $bin {
			dict with value {
			    dict for {type num} $count {
				dict set bin $id count $type\
				    [expr {double($num)/$area}]
			    }
			} 
		    }
		}
	    }
	}
    }    
    return $bins
}


#	spinogram {
#       ## this is wrong, as every bin should have the same number of observations
#	    dict with bins {
#		dict for {id value} $bin {
#		    dict with value {
#			set all [dict get $count all]
#			dict for {type num} $count {
#			    dict set bin $id count $type\
#				[expr {double($num)/$all}]
#			}
#		    } 
#		}
#	    }	    
#	}
