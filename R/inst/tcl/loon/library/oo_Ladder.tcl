
oo::class create loon::classes::Ladder {

    superclass ::loon::classes::Inspector2

    variable canvas y_mouse dyStep y1Ladder xBulletX xBulletY
	#cache_xlabel cache_ylabel cache_x cache_y\
	#cache_minX cache_maxX cache_minY cache_maxY
    
    ## dyStep : number of pixel for 1 unit alpha
    

    constructor {Path} {

	set canvas ""
	
	set y_mouse 0
	set dyStep 1
	set y1Ladder 1
	set xBulletX 1
	set xBulletY 1
	
	# set cache_xlabel ""
	# set cache_ylabel ""
	# set cache_x ""
	# set cache_y ""
	# set cache_minX ""
	# set cache_maxX ""
	# set cache_minY ""
	# set cache_maxY ""

	next $Path
	
	my New_state alphaX double 1 0
	my New_state alphaY double 1 0
	
	my New_state from integer 1 -5
	my New_state to integer 1 5
	
	bind $canvas <Configure> "[self namespace]::my redraw"
	
	bind $canvas <Button-1> "[self namespace]::my ButtonPress %y"
	$canvas bind xbullet <B1-Motion> "[self namespace]::my DragBullet x %y"
	$canvas bind ybullet <B1-Motion> "[self namespace]::my DragBullet y %y"

	my redraw

    }
    

    method ButtonPress {y} {
	set y_mouse $y
    }
    
    method DragBullet {which y} {
	my variable alphaX alphaY from to
	
	set dy [expr {$y - $y_mouse}]
	set y_mouse $y
	
	if {$dy eq 0} {return}

	switch -- $which {
	    x {
		set new_alphaX [expr {$alphaX - $dy/double($dyStep)}]
		if {$new_alphaX < $from} {
		    set new_alphaX $from
		} elseif {$new_alphaX > $to} {
		    set new_alphaX $to
		}
		my configure -alphaX $new_alphaX
	    }
	    y {
		set new_alphaY [expr {$alphaY - $dy/double($dyStep)}]
		if {$new_alphaY < $from} {
		    set new_alphaY $from
		} elseif {$new_alphaY > $to} {
		    set new_alphaY $to
		}
		my configure -alphaY $new_alphaY 
	    }
	}
	
    }
    
    
    method CheckNewActivewidget {widget} {
	if {![info object isa typeof $widget\
		  "::loon::classes::Scatterplot_Widget"]} {
	    error "$widget is not a Scatterplot Widget."
	}
    }

    method RegisterActivewidget {} {
	my variable activewidget
	
	next
	
	if {$activewidget ne ""} {
	    # set xlabel [$activewidget cget -xlabel]
	    # if {$xlabel eq ""} {
	    # 	set xlabel "x"
	    # }

	    # set ylabel [$activewidget cget -ylabel]
	    # if {$ylabel eq ""} {
	    # 	set ylabel "y"
	    # }
	    
	    my configure -alphaX 0
	    my configure -alphaY 0

#	    my ActivewidgetEvents x y

	}
	
    }

    method HookAfterStatesSet {} {
	my variable changedStates activewidget

	next
	
	set num 0
	foreach state {alphaX alphaY} {
	    if {$state in $changedStates} {
	 	incr num
		set has_$state TRUE
	    } else {
		set has_$state FALSE
	    }
	}
	
	# if {$has_alphaX && $has_alphaY} {
	#     if {$activewidget ne ""} {
		
	# 	set xt [my PowerTransX]
	# 	set mm [::loon::listfns::MinMax $xt] 
	# 	set minX [lindex $mm 0]
	# 	set maxX [lindex $mm 1]
	# 	set zoomX [expr {($cache_maxX - $cache_minX)/double($maxX - $minX)}]
		
	# 	set yt [my PowerTransY]
	# 	set mm [::loon::listfns::MinMax $yt] 
	# 	set minY [lindex $mm 0]
	# 	set maxY [lindex $mm 1]
	# 	set zoomY [expr {($cache_maxY - $cache_minY)/double($maxY - $minY)}]
		
		
	# 	$activewidget configure\
	# 	    -xTemp $xt -yTemp $yt\
	# 	    -panX $minX -zoomX $zoomX\
	# 	    -panY $minY -zoomY $zoomY

	#     }
	# } elseif {$has_alphaX} {
	#     if {$activewidget ne ""} {

	# 	set xt [my PowerTransX]
	# 	set mm [::loon::listfns::MinMax $xt] 
	# 	set minX [lindex $mm 0]
	# 	set maxX [lindex $mm 1]
	# 	set zoomX [expr {($cache_maxX - $cache_minX)/double($maxX - $minX)}]
		
	# 	$activewidget configure -xTemp $xt -panX $minX -zoomX $zoomX		
	#     }
	# } elseif {$has_alphaY} {
	#     if {$activewidget ne ""} {

	# 	set yt [my PowerTransY]
	# 	set mm [::loon::listfns::MinMax $yt] 
	# 	set minY [lindex $mm 0]
	# 	set maxY [lindex $mm 1]
	# 	set zoomY [expr {($cache_maxY - $cache_minY)/double($maxY - $minY)}]
		
	# 	$activewidget configure -yTemp $yt -panY $minY -zoomY $zoomY
	#     }
	# }
	
	if {[llength $changedStates] > $num} {
	    my redraw
	} else {
	    my UpdatePos
	}

    }
    
    method PowerTransX {} {
	my variable alphaX
	
	if {$alphaX eq 0} {return $cache_x}
	
	set out {}
	set dalpha [expr {double($alphaX)}]
	foreach x $cache_x {
	    lappend out [expr {(pow($x,$dalpha)-1.0)/$dalpha}]
	}
	return $out
    }

    method PowerTransY {} {
	my variable alphaY
	
	if {$alphaY eq 0} {return $cache_y}
	
	set out {}
	set dalpha [expr {double($alphaY)}]
	foreach y $cache_y {
	    lappend out [expr {(pow($y,$dalpha)-1.0)/$dalpha}]
	}
	return $out
    }
    

    method ActivewidgetEvents {args} {
	## args is events
	my variable activewidget
	
	## TODO: fix, sometimes the events come nested
	if {[llength $args] eq 1} {
	    set args {*}$args
	}

	# if {"x" in $args} {
	#     set cache_x [$activewidget cget -x]
	#     set mm [::loon::listfns::MinMax $cache_x]
	#     set cache_minX [lindex $mm 0]
	#     set cache_maxX [lindex $mm 1] 
	# }
	# if {"y" in $args} {
	#     set cache_y [$activewidget cget -y]
	    
	#     set mm [::loon::listfns::MinMax $cache_y]
	#     set cache_minY [lindex $mm 0]
	#     set cache_maxY [lindex $mm 1]	   	    
	# }
       	
    }

    
    method Make {} {
	my variable path
	
	frame $path -class LoonLadder
	set canvas [canvas ${path}.canvas -bg white -width 180 -height 400]
	pack $canvas -fill y -expand TRUE
    }
    
    method redraw {} {
	
	my variable from to

	$canvas delete all
	
	set width [winfo width $canvas]
	set height [winfo height $canvas]

	set x0 60
	set x1 [expr {$width - 40}]
	set y0 80
	set y1Ladder [expr {$height -80}]
	
	set ladder_height [expr {$y1Ladder - $y0}]
	set ladder_width [expr {$x1 - $x0}]

	set xBulletY [expr {$ladder_width/2.0 - 20 + $x0}]
	set xBulletX [expr {$x1 - $ladder_width/2.0 + 20}]
	
	$canvas create line $x0 $y0 $x0 $y1Ladder 
	$canvas create line $x1 $y0 $x1 $y1Ladder
	
	
	set nsteps [expr {$to - $from}]
	
	set dyStep [expr {$ladder_height/double($nsteps)}]

	set xlab 50

	set ystep $y0
	set lab $to

	for {set i 0} {$i <= $nsteps} {incr i} {
	    
	    $canvas create line $x0 $ystep $x1 $ystep
	    $canvas create text $xlab $ystep -text $lab -anchor center -justify right
	    
	    incr lab -1
	    set ystep [expr {$ystep + $dyStep}]
	}
	
	$canvas create oval -16 -16 16 16 -fill steelblue -outline steelblue\
	    -tag [list bullet xbullet circleX]
	$canvas create text 0 0 -text x -anchor c -justify center\
	    -tag [list bullet xbullet labelX]
	
	$canvas create oval -16 -16 16 16 -fill thistle -outline thistle\
	    -tag [list bullet ybullet circleY]
	$canvas create text 0 0 -text y -anchor c -justify center\
	    -tag [list bullet ybullet labelY]

	my UpdatePos
	

	
    }
    
    method UpdatePos {} {
	my variable from alphaX alphaY

	set xypos [expr {$y1Ladder - ($alphaX - double($from))*$dyStep}]
	set yypos [expr {$y1Ladder - ($alphaY - double($from))*$dyStep}]
	
	$canvas coords "labelX" 0 0
	$canvas coords "circleX" -16 -16 16 16
	$canvas coords "labelY" 0 0
	$canvas coords "circleY" -16 -16 16 16
	
	$canvas move xbullet $xBulletX $xypos
	$canvas move ybullet $xBulletY $yypos
	       	
	update idletasks

    }
    

}

