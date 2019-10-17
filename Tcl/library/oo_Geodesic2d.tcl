
oo::class create ::loon::classes::Geodesic2d {
    
    superclass ::loon::classes::withScaledData\
	::loon::classes::Context2d
    
    
    variable plot pi
    
    constructor {Graph Navigator} {
	
	set pi 3.1415926535897931
	
	next $Graph $Navigator

    }
    
    method HookAfterStatesSet {} {
	next
	my variable changedStates
	if {"data" in $changedStates ||\
		"scaling" in $changedStates} {
	    my Update
	}
    }
    
    method Update {} {
	my variable n data curXvars curYvars curP scaled_data curFrom curTo

	if {$n eq 0 || $scaled_data eq ""} {return}
	
	set DX [llength $curXvars]
	set DY [llength $curYvars]
	
	set D [expr {$DX + $DY}]
	
	set angle [expr {$curP*$pi/2.0}]
	set cosA [expr {cos($angle)}]
	set sinA [expr {sin($angle)}]
	
	set vec [list $cosA $sinA]
	
	switch -- $D {
	    0 {
		set X ""
		set Y ""
		set xlabel ""
		set ylabel ""
	    }
	    2 {
		
		set X [dict get $scaled_data $curXvars]
		set xlabel $curXvars
		set Y [dict get $scaled_data $curYvars]
		set ylabel $curYvars
		
	    }
	    3 {
		## rigid rotation
		if {$DX eq 1} {
		    set X [dict get $scaled_data $curXvars]
		    
		    set xlabel $curXvars
		    
		    set Y [::loon::listfns::comp2d\
			       [dict get $scaled_data [lindex $curYvars 0]]\
			       [dict get $scaled_data [lindex $curYvars 1]]\
			       $vec]
		    
		    set ylabel [format "%s %.3f + %s %.3f"\
				    [lindex $curYvars 0] $cosA\
				    [lindex $curYvars 1] $sinA]
		} else {
		    set Y [dict get $scaled_data $curYvars]
		    
		    set ylabel $curYvars
		    
		    set X [::loon::listfns::comp2d\
			       [dict get $scaled_data [lindex $curXvars 0]]\
			       [dict get $scaled_data [lindex $curXvars 1]]\
			       $vec]
		    
		    set xlabel [format "%s %.3f + %s %.3f"\
				    [lindex $curXvars 0] $cosA\
				    [lindex $curXvars 1] $sinA]
		    
		}

	    }
	    4 {
		set X [::loon::listfns::comp2d\
			   [dict get $scaled_data [lindex $curXvars 0]]\
			   [dict get $scaled_data [lindex $curXvars 1]]\
			   $vec]
		
		set xlabel [format "%s %.3f + %s %.3f"\
				    [lindex $curXvars 0] $cosA\
				    [lindex $curXvars 1] $sinA]
		
		set Y [::loon::listfns::comp2d\
			   [dict get $scaled_data [lindex $curYvars 0]]\
			   [dict get $scaled_data [lindex $curYvars 1]]\
			   $vec]

		set ylabel [format "%s %.3f + %s %.3f"\
				    [lindex $curYvars 0] $cosA\
				   [lindex $curYvars 1] $sinA]
		
	    }
	    default {
		error "wrong dimension: $D, $curXvars - $curYvars"
	    }
	}

	my EvalCommand $X $Y $xlabel $ylabel $curFrom $curTo $curP

    }

    method EvalCommand {x y xlabel ylabel from to p} {
	my variable command substitutions
	
	
	if {[llength $command] ne 0} {
	    uplevel #0 [string map [list {*}$substitutions\
					%xlabel [list $xlabel]\
					%ylabel [list $ylabel]\
					%x [list $x]\
					%y [list $y]\
					%from $from %to $to %p $p] $command]
	}
    }
    
    
}
