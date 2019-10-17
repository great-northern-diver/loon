

oo::class create ::loon::classes::Context2d {
    
    superclass ::loon::classes::Context 
    
    
    variable curXvars curYvars curFrom curTo curP
    
    constructor {Graph Navigator} {
	
	set curXvars ""
	set curYvars ""
	set curFrom ""
	set curTo ""
	set curP 0
	
	next $Graph $Navigator
#	my New_state data string 1 ":"
#	my New_state scaling factor 1 ":"

	my New_state interchange4d boolean 1 FALSE

	my SetStateDescription interchange4d\
	    "if FALSE then a transition on edget from note A:B to C:D will map to A>C and B>D, otherwise from A>D and B>C"
	
    }

    method HookAfterStatesSet {} {
	my variable changedStates
	next

	if {"interchange4d" in $changedStates} {
	    if {[llength $curXvars] eq 2 && [llength $curYvars] eq 2} {
		set tmp [lindex $curXvars 1]
		lset curXvars 1 [lindex $curYvars 1]
		lset curYvars 1 $tmp
		my Update
	    }	    
	}
    }
    
    method InitNavigator {} {
	my variable navigator navigatorObj

	if {$navigator eq ""} {
	    set curXvar ""
	    set curYvar ""
	} else {
	    set curFrom [$navigatorObj cget -from]

	    ## need to ensure to get into correct logic in
	    ## NavigatorUpdate:
	    ##  - navigator on edge: leave node
	    ##  - navigator on node: arrive at old node
	    ## works
	    
	    #set curTo [$navigatorObj cget -to]
	    #set curP [$navigatorObj cget -proportion]
	    	    
	    my NavigatorUpdate
	}
	
    }

    method NavigatorUpdate {} {
	my variable navigatorObj separator
	
	if {$navigatorObj eq ""} {return}
	
	set from [lindex [$navigatorObj cget -from] end]
	set to [lindex [$navigatorObj cget -to] 0]
	set p [$navigatorObj cget -proportion]
	
	set fromVars [::loon::listfns::mcsplit $from $separator]
	set toVars [::loon::listfns::mcsplit $to $separator]
	
	if {[llength $fromVars] ni {0 2}} {
	    error "context2d: from node $from does not separate into 2 spaces\
                   using the sparator \"$separator\""
	}
	if {[llength $toVars] ni {0 2}} {
	    error "context2d: to node $to does not separate into 2 spaces\
                   using the sparator \"$separator\""
	}
	
	## puts "Context $from $to $p"
	
	## We have 5 situations:
	## - leave node
	## - same transition
	## - arrive at new adjacent node
	## - arrive at old node
	## - switch node

	set xvars ""
	set yvars ""
	
	set space [lsort -unique [concat $fromVars $toVars]]
	set dim [llength $space]
	
	## keep variables on same axis
	switch -- $dim {
	    0 {
		

	    }
	    2 {
		set var1 [lindex $space 0]
		set var2 [lindex $space 1]
		if {$var1 ni $curYvars} {
		    set xvars $var1
		    set yvars $var2
		} else {
		    set xvars $var2
		    set yvars $var1
		}
	    }
	    3 {
		if {[lindex $fromVars 0] in $toVars} {
		    set sharedVar [lindex $fromVars 0]
		    set fromVar [lindex $fromVars 1]
		} else {
		    set sharedVar [lindex $fromVars 1]
		    set fromVar [lindex $fromVars 0]
		}
		if {[lindex $toVars 0] ne $sharedVar} {
		    set toVar [lindex $toVars 0]
		} else {
		    set toVar [lindex $toVars 1]
		}
		
		if {$sharedVar ni $curYvars} {
		    set xvars $sharedVar
		    set yvars [list $fromVar $toVar]
		} else {
		    set xvars [list $fromVar $toVar]
		    set yvars $sharedVar
		}
	    }
	    4 {
		my variable interchange4d

		## Assume A:B > C:D
		set A [lindex $fromVars 0]
		set B [lindex $fromVars 1]
		set C [lindex $toVars 0]
		set D [lindex $toVars 1]
		
		set curx_0 [lindex $curXvars 0]
		
		if {!$interchange4d} {
		    if {$B eq $curx_0} {
			# (B,A) > (D,C)
			set xvars [list $B $D]
			set yvars [list $A $C]
		    } else {
			# default (A,B) > (C,D) 
			set xvars [list $A $C]
			set yvars [list $B $D]
		    }
		} else {
		    if {$B eq $curx_0} {
			# (B,A) > (C,D)
			set xvars [list $B $C]
			set yvars [list $A $D]
		    } else {
			# (A,B) > (D, C)
			set xvars [list $A $D]
			set yvars [list $B $C]
		    }
		}
	    }
	    default {
		error "space \"$space\" is not 2, 3, or 4 dimensional."
	    }
	}
	
	## puts "---: $xvars, $yvars"
	
	set curFrom $from
	set curTo $to
	set curP $p
	set curXvars $xvars
	set curYvars $yvars

	if {$dim ne 0} {
	    my Update
	}
    }

    
    method Update {} {
	my EvalCommand $curXvars $curYvars $curFrom $curTo $curP
    }
    

    method EvalCommand {xvars yvars from to p} {
	my variable command substitutions
	
	if {[llength $command] ne 0} {
	    uplevel #0 [string map [list {*}$substitutions\
					%xvars [list $xvars]\
					%yvars [list $yvars]\
					%from $from\
					%to $to\
					%p $p] $command]
	}
    }

}
