
namespace eval loon::scales {

    #
    # An Extension of Wilkinson’s Algorithm
    # for Positioning Tick Labels on Axes
    # 2010 Justin Talbot, Sharon Lin, and Pat Hanrahan
    # Tranlated from R package "labeling"
    #

    proc extended {dmin dmax m\
		       {Q {1 5 2 2.5 4 3}}\
		       {only.loose FALSE}\
		       {w {.25 .2 .5 .05}}} {
#	puts "dmin: $dmin\ndmax: $dmax\nm: $m"
	# I just use a very small number
	set eps 2e-10
	
	if {$dmin > $dmax} {
	    set tmp $dmin
	    set dmin $dmax
	    set dmax $tmp
	}

	if {$m < 2} {
	    # add .0 if one string is a double and one is an integer
	    if {!([string is integer $dmin] && [string is integer $dmax])} {
		set dmin [expr {double($dmin)}]
		set dmax [expr {double($dmax)}]
	    }
	    
	    return [list [format "%.3f" $dmin] [format "%.3f" $dmax]]
	}

	if {[expr {$dmax - $dmin} < $eps]} {
	    #if the range is near the floating point limit,
	    #let seq generate some equally spaced steps.
	    set seq {}
	    set delta [expr {$dmax - $dmin}]
	    for {set i 0} {$i < $m} {incr i} {
		lappend seq [expr {$i/$m*$delta}]
	    }
	    return $seq
	}

	set n [llength $Q]

	set best(score) -2
	set best(lmin) none
	set best(lmax) none
	set best(lstep) none
	

	set j 1
	while {$j ne -1} {
	    #	puts "j = $j"

	    foreach q $Q {
		set sm [simplicity.max $q $Q $j]
		
		if {[expr {[lindex $w 0]*$sm\
			       + [lindex $w 1]\
			       + [lindex $w 2]\
			       + [lindex $w 3]}] < $best(score)} {
		    
		    set j -2
		    break
		}

		set k 2
		while {$k ne "done"} {
		    # loop over tick counts
		    
		    set dm [density.max $k $m]
		    #		puts "k=$k, dm=$dm"
		    
		    if {[expr {[lindex $w 0]*$sm\
				   + [lindex $w 1]\
				   + [lindex $w 2] * $dm\
				   + [lindex $w 3]}] < $best(score)} {
			break
		    }

		    set delta [expr {($dmax - $dmin)/($k+1.0)/$j/$q}]
		    set z [expr {ceil(log10($delta))}]
		    
		    #		puts "delta=$delta, z=$z"

		    while {$z ne "done"} {
			set step [expr {double($j)*$q*pow(10,$z)}]		   
			
			set cm [coverage.max $dmin $dmax [expr {$step*($k-1.0)}]]
			
			#		    puts "z=$z, step=$step, cm=$cm"
			if {[expr {[lindex $w 0]*$sm\
				       + [lindex $w 1] * $cm\
				       + [lindex $w 2] * $dm\
				       + [lindex $w 3]}] < $best(score)} {
			    break
			}
			
			set min_start [expr {int(floor(double($dmax)/$step)*$j-($k-1.0)*$j)}]
			set max_start [expr {int(ceil(double($dmin)/$step)*$j)}]

			#		    puts "min_start=$min_start, max_start=$max_start"
			if {$min_start > $max_start} {
			    set z [expr {$z + 1}]
			    continue
			}
			
			for {set start $min_start} {$start <= $max_start} {incr start} {
			    set lmin [expr {$start * double($step)/$j}]
			    set lmax [expr {$lmin + $step*($k-1.0)}]
			    set lstep $step
			    
			    #			puts "lmin:$lmin, lmax:$lmax, lstep:$lstep"
			    
			    set s [simplicity $q $Q $j $lmin $lmax $lstep]
			    set c [coverage $dmin $dmax $lmin $lmax]
			    set g [density $k $m $dmin $dmax $lmin $lmax]
			    set l [legibility $lmin $lmax $lstep]			
			    
			    set score [expr {[lindex $w 0]*$s\
						 + [lindex $w 1] * $c\
						 + [lindex $w 2] * $g\
						 + [lindex $w 3] * $l}]
			    
			    #			puts "score:$score, s:$s, c:$c, g:$g, l:$l"
			    
			    if {$score > $best(score)\
				    && (${only.loose} \
					    || ($lmin >= $dmin  && $lmax <= $dmax))} {
				set best(lmin) $lmin
				set best(lmax) $lmax
				set best(lstep) $lstep
				set best(score) $score
			    }
			}
			set z [expr {$z + 1}]
		    }
		    set k [expr {$k +1}]
		}
	    }
	    set j [expr {$j + 1}]
	}

#	puts "min: $best(lmin)\nmax: $best(lmax)\nstep: $best(lstep)"

	set seq {}
	if {"none" in [list $best(lmin) $best(lmax) $best(lstep)]} {
	    set seq [list $dmin $dmax]
	} else {
	    set lmax $best(lmax)
	    set lstep $best(lstep)
	    
	    set val $best(lmin)
	    while {$val < $lmax} {
		lappend seq $val
		set val [expr {$val + $lstep}]
	    }
	    lappend seq $lmax
	}
	return $seq
    }

    proc simplicity {q Q j lmin lmax lstep} {
	# I just use a very small number
	set eps 2e-10
	
	set n [llength $Q]
	set i [lsearch -exact $Q $q]
	if {$i eq -1} {
	    error "q=$q is not in Q=$Q"
	}
	if {(fmod($lmin,$lstep) < $eps\
		 || ($lstep - fmod($lmin,$lstep)) < $eps)\
		&& $lmin <= 0 && $lmax >=0} {
	    set v 1
	} else {
	    set v 0
	}
	
	return [expr {1.0 - $i/($n-1.0) - $j + $v}]    
    }

    proc simplicity.max {q Q j} {
	set n [llength $Q]
	set i [lsearch -exact $Q $q]
	if {$i eq -1} {
	    error "q=$q is not in Q=$Q"
	}
	set v 1

	return [expr {1.0 - $i/($n-1.0) - $j + $v}]
    }

    proc density { k m dmin dmax lmin lmax } {
	set r [expr {($k-1.0)/($lmax-$lmin)}]
	set rt [expr {($m-1.0)/(max($lmax,$dmax)-min($dmin,$lmin))}]
	
	return [expr {2.0 - max($r/$rt, $rt/$r)}]
    }

    proc density.max {k m} {

	if {$k > $m} {
	    set result [expr {2 - ($k-1.0)/($m-1.0)}]
	} else {
	    set result 1
	}

	return $result
    }

    proc coverage { dmin dmax lmin lmax } {
	set range [expr {$dmax-$dmin}]
	return [expr {1.0 - 0.5 * (pow($dmax-$lmax,2)+pow($dmin-$lmin,2))/pow(0.1*$range,2)}]
    }

    proc coverage.max {dmin dmax span} {
	set range [expr {$dmax - $dmin}]
	
	if {$span > $range} {
	    set half [expr {($span-$range)/2.0}]
	    # why 2*half^2
	    # 1 - 0.5 * (half^2 + half^2) / ((0.1 * range)^2)
	    set result [expr {1.0- pow($half,2)/pow(.1*$range,2)}]
	} else {
	    set result 1
	}
	return $result
    }

    proc legibility {lmin lmax lstep} {
	# this code was not implemented in the R package
	return 1
    }

}
