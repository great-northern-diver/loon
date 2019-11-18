
oo::class create loon::classes::MinMaxScale {

    superclass ::loon::classes::Inspector2
    
    variable path canvas from to min max resolution\
	b_w b_e b_s b_n slider_width slider_width2 slider_peak pad_text\
	current_slider n_pix_per_res n_pix_per_res2\
	mouse_x
   
    constructor {Path} {


	set mouse_x 0
	
	set current_slider ""
	set n_pix_per_res 1
	set n_pix_per_res2 0.5
	
	set b_w 25
	set b_e 25
	set b_s 25
	set b_n 25
	set slider_peak 5
	set slider_width 16
	set slider_width2 8
	set pad_text 3
	
	next $Path
	
	my New_state from double 1 0
	my New_state to double 1 1
	my New_state min double 1 0
	my New_state max double 1 1
	my New_state resolution positive_double 1 0.01
	#my New_state orient factor 1 horizontal {horizontal vertical}
	#my New_state showvalues orient boolean 1 TRUE
	my New_state command string 1 ""

	my SetStateDescription from\
	    "from value of scale"
	my SetStateDescription to\
	    "to value of scale"
	my SetStateDescription min\
	    "position of min slider"
	my SetStateDescription max\
	    "position of max slider"
	my SetStateDescription resolution\
	    "resolution for the scale"
	my SetStateDescription command\
	    "callback that is evaluated with any state change"
	
    }

    method EvalConfigure {} {
	my variable confDict

	next

	## check that from <= min <= max <= to
	set hasValue FALSE
	foreach val {min max from to} {
	    if {[dict exists $confDict arg_$val]} {
		set hasValue TRUE
		set val_$val [dict get $confDict arg_$val]
	    } else {
		set val_$val [set $val]
	    }
	}

	if {$hasValue} {

	    if {$val_from > $val_min} {
		error "min > from: $val_min > $val_from"		
	    }
	    if {$val_min > $val_max} {
		error "min > max: $val_min > $val_max"
	    }
	    if {$val_max > $val_to} {
		error "max > to: $val_max > $val_to"
	    }	    
	}		
	
    }
    method EvalCommand {} {
	my variable command path
	if {$command ne ""} {
	    uplevel #0 [string map [list %W $path %min $min %max $max] $command]
	}
	
    }
    
    method Make {} {
	frame $path -class LoonMinMaxScale
	set canvas [canvas ${path}.canvas -height 70 -width 200]
	
	pack $canvas -fill both -expand TRUE -side top

	bind $canvas <Configure> "[self namespace]::my Redraw"
	$canvas bind "min||max||drag" <ButtonPress-1>\
	    "[self namespace]::my SelectSlider %x"
	bind $canvas <ButtonRelease>\
	    "[self namespace]::my ReleaseSlider"
	bind $canvas <Button1-Motion>\
	    "[self namespace]::my DragSlider %x"
    }  

    method HookAfterStatesSet {} {
	my variable changedStates
	if {[llength $changedStates] > 0} {
	    my Redraw	    
	}
	if {"min" in $changedStates || "max" in $changedStates} {
	    my EvalCommand
	}
	
    }

    method Redraw {} {

	set w [winfo width $canvas]
	set h [winfo height $canvas]
	
	$canvas delete all

	set x0 $b_w
	set x1 [expr {$w - $b_e}]

	set y0 $b_n
	set y1 [expr {$h - $b_s}]
	
	$canvas create rect $x0 $y0 $x1 $y1 -fill darkgrey

	
	set x_min [expr {$b_e + $slider_width}]
	set x_max [expr {$w - $b_w - $slider_width}]
	set dx [expr {double($x_max - $x_min)}]

	## Location of the min and slider
	set loc_min [expr {$x_min + ($min-$from)/($to - $from)*$dx}]
	set loc_max [expr {$x_min + ($max-$from)/($to - $from)*$dx}]

	$canvas create rect\
	    $loc_min $y0 $loc_max $y1\
	    -fill "#C6DCE6" -tag drag
	
	$canvas create rect\
	    [expr {$loc_min - $slider_width}] $y0\
	    $loc_min [expr {$y1 + $slider_peak}]\
	    -fill gray -tag min

	$canvas create text\
	    [expr {$loc_min - $slider_width2}]\
	    [expr {$y1 + $slider_peak + $pad_text}]\
	    -text [format "%.5g" $min] -anchor n -justify center
	
	$canvas create rect\
	    $loc_max [expr {$y0 - $slider_peak}]\
	    [expr {$loc_max + $slider_width}] $y1\
	    -fill gray -tag max
	
	$canvas create text\
	    [expr {$loc_max + $slider_width2}]\
	    [expr {$y0 - $slider_peak - $pad_text}]\
	    -text [format "%.5g" $max] -anchor s -justify center

	set n_pix_per_res [expr {$dx/($to - $from)*$resolution}]
	set n_pix_per_res2 [expr {$n_pix_per_res/2.0}]

    }
    

    method SelectSlider {x} {

	set mouse_x $x
	set tag [lindex [$canvas gettags current] 0]
	switch -- $tag {
	    min {
		set current_slider "min"
	    }
	    max {
		set current_slider "max"
	    }
	    drag {
		set current_slider "drag"
	    }
	    default {
		set current_slider ""
	    }
	}
    } 

    method ReleaseSlider {} {
	set current_slider ""
    }  

    method DragSlider {x} {
	switch -- $current_slider {
	    min {
		set x_loc [expr {$x - $b_e - $slider_width + $slider_width2 + $n_pix_per_res2}]
		set mul [expr {int($x_loc / $n_pix_per_res )}]
		set x_min [expr {$from + $mul*$resolution}]

		if {$x_min < $from} {
		    set x_min $from
		} elseif {$x_min > $max} {
		    set x_min $max
		} 
		my configure -min $x_min
	    }
	    max {
		set x_loc [expr {$x - $b_e - $slider_width - $slider_width2 + $n_pix_per_res2}]
		set mul [expr {int($x_loc / $n_pix_per_res )}]
		set x_max [expr {$from + $mul*$resolution}]

		if {$x_max > $to} {
		    set x_max $to
		} elseif {$x_max < $min} {
		    set x_max $min
		}		
		my configure -max $x_max
	    }
	    drag {
		
		set m_dx [expr {$x - $mouse_x}]

		#puts "m_dx=$m_dx"
		
		set x_change [expr {$m_dx/double($n_pix_per_res)*$resolution}]

		#puts [format "x_change=%.4f" $x_change]

		set x_max [expr {$max + $x_change}]
		set x_min [expr {$min + $x_change}]
		
		if {$x_max > $to} {
		    set x_change [expr {$to - $max}]
		} elseif {$x_min < $from} {
		    set x_change [expr {$from - $min}]
		}

		#puts [format "   after=%.4f" $x_change]
		
		my configure -min [expr {$min + $x_change}] -max [expr {$max + $x_change}]
		
	    }
	}
	
	set mouse_x $x
    }
    
}
