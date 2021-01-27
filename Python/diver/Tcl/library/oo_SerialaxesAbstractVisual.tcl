
oo::class create loon::classes::SerialaxesAbstractVisual {

    superclass ::loon::classes::Visual

    variable model ids

    constructor {Model args} {
	set ids "noinit"

	set model $Model

	next {*}$args
    }

    method selectedAbove {} {
	my variable canvas ids visualid

	foreach i [::loon::listfns::subsetLogical $ids [set ${model}::selected]] {
	    $canvas raise $i $visualid
	}

    }


    method recolor {} {

	my variable canvas ids visualid

	set colors [set ${model}::color]
	set sel_color $::loon::Options(select-color)

	set i 0
	foreach s [set ${model}::selected] {
	    if {$s} {
		lset colors $i $sel_color
	    }
	    incr i
	}

	if {[set ${model}::showArea]} {
	    foreach color $colors i $ids {
		if {$i ne "-1"} {
		    $canvas itemconfigure $i -fill $color -outline $color
		}
	    }
	} else {
	    foreach i $ids color $colors {
		if {$i ne "-1"} {
		    $canvas itemconfigure $i -fill $color
		}
	    }
	}

	my selectedAbove

    }

    method updateCoords {} {
	my redraw
    }

    method itemIndicesIntersectWithLine {x0 y0 x1 y1} {
	## Unfortunately there is no easy way to find the intersecting
	## items with the line...
	my variable canvas ids


	if {$x1 > $x0} {
	    set X0 $x0; set X1 $x1
	} else {
	    set X1 $x0; set X0 $x1
	}
	if {$y1 > $y0} {
	    set Y0 $y0; set Y1 $y1
	} else {
	    set Y1 $y0; set Y0 $y1
	}

	if {$x1 >= $x0 && $y1 >= $y0} {
	    set dir "++"
	} elseif {$x1 <= $x0 && $y1 <= $y0} {
	    set dir "--"
	} elseif {$x1 >= $x0 && $y1 <= $y0} {
	    set dir "+-"
	} else {
	    set dir "-+"
	}


	set ind [::loon::listfns::setIntersection $ids\
		     [$canvas find overlapping $X0 $Y0 $X1 $Y1]]


	set iter 1
	set p [expr {1.0/pow(2,$iter)}]
	set Dx [expr {($x1 - $x0)}]
	set Dy [expr {($y1 - $y0)}]

	set dx [expr {$Dx*$p}]
	set dy [expr {$Dy*$p}]

	#$canvas delete temp

	while {abs($dx) > 2 && abs($dy) > 2 && [llength $ind] > 0} {
	    set sx [expr {$dx/2.0}]
	    set sy [expr {$dy/2.0}]

	    set x0s $x0
	    set y0s $y0
	    set x1s [expr {$x0s + $dx}]
	    set y1s [expr {$y0s + $dy}]


	    set tind {}
	    for {set i 0} {$i < [expr {2*pow(2,$iter) - 1}]} {incr i} {
		switch -- $dir {
		    "++" {
			#puts " ++: $x0s $y0s $x1s $y1s"
			lappend tind {*}[$canvas find overlapping $x0s $y0s $x1s $y1s]
		    }
		    "--" {
			#puts " --: $x1s $y1s $x0s $y0s"
			lappend tind {*}[$canvas find overlapping $x1s $y1s $x0s $y0s]
		    }
		    "+-" {
			#puts " +-: $x0s $y1s $x1s $y0s"
			lappend tind {*}[$canvas find overlapping $x0s $y1s $x1s $y0s]
		    }
		    "-+" {
			#puts " -+: $x1s $y0s $x0s $y1s"
			lappend tind {*}[$canvas find overlapping $x1s $y0s $x0s $y1s]
		    }
		}
		#$canvas create rect $x0s $y0s $x1s $y1s -fill "" -outline orange -tag temp

		set x0s [expr {$x0s+$sx}]
		set y0s [expr {$y0s+$sy}]
		set x1s [expr {$x0s + $dx}]
		set y1s [expr {$y0s + $dy}]

	    }
	    set ind [::loon::listfns::setIntersection $ind $tind]

	    incr iter
	    set p [expr {1.0/pow(2,$iter)}]
	    set dx [expr {$Dx*$p}]
	    set dy [expr {$Dy*$p}]
	}

	#$canvas create text 40 40 -text "[expr {2*pow(2,$iter) - 1}]" -fill orange\
	 #   -tag temp

	set pointind {}
	if {[llength $ind]>0} {
	    ## replace ind with the indecies
	    foreach i $ind {
		set j [lsearch -exact $ids $i]
		if {$j ne "-1"} {
		    lappend pointind $j
		} else {
		    error "Collision detection error"
		}
	    }
	}

	return [lsort -unique $pointind]
    }

    method clear {} {
	next
	set ids "noinit"
    }




}
