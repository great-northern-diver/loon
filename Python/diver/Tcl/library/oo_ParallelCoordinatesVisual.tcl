oo::class create loon::classes::ParallelCoordinatesVisual {
    
    superclass ::::loon::classes::SerialaxesAbstractVisual
    
    
    method redraw {} {
	my variable canvas id visualid model
	
	if {$model eq ""} {return}

	if {$id ne "noinit"} {
	    my clear
	}
	
	if {[llength [set ${model}::glyphs]] eq 0} {
	    return
	}
	
	set width [winfo width $canvas]
	set height [winfo height $canvas]
	
	set p [llength [set ${model}::sequence]]

	if {[set ${model}::showLabels]} {
	    set title [set ${model}::title]
	} else {
	    set title ""
	}
	
	set font "Arial 14"
	
	if {$title ne ""} {
	    set ymargin_top 100
	} else {
	    set ymargin_top 50
	}
	
	set ymargin_bottom 50
	set xmargin 80
	
	## Dimensions
	set axes_length [expr {$height-$ymargin_top-$ymargin_bottom}]
	set axes_distance [expr {($width - 2.0*$xmargin)/($p - 1)}]
	set plot_width [expr {$width - 2*$xmargin}]
	
	if {$plot_width < 20 || $axes_length < 20} {
	    my clear
	    $canvas create text [expr {$width/2.0}] [expr {$height/2.0}]\
		-anchor center\
		-justify center\
		-text "Window is\ntoo small"\
		-tag $visualid\
		-font $font
		return
	}
	
	foreach state {showGuides showAxes showAxesLabels showLabels showArea color} {
	    set $state [set ${model}::$state] 
	}
	
	set sel_color $::loon::Options(select-color)
	
	set i 0
	foreach s [set ${model}::selected] {
	    if {$s} {
		lset color $i $sel_color
	    } 
	    incr i
	}
	
	set direction 1
	

	
	set xAxes {}
	for {set i 0} {$i < $p} {incr i} {
	    lappend xAxes [expr {$i*$axes_distance}]
	}

	
	#puts "axes_length: $axes_length, axes_distance: $axes_distance, plot_width: $plot_width "
	
	# Guides
	if {$showGuides} {
	    $canvas configure -bg $::loon::Options(canvas_bg_guides)
	    
	    set minGuideDist $::loon::Options(fixed_guide_distance)
	    
	    set ng [expr {max(int($axes_length/$minGuideDist),1)}]
	    set gd [expr {double($axes_length/$ng)}]
	    incr ng -1

	    set ys 0
	    for {set i 1} {$i <= $ng} {incr i} {
		lappend ys [expr {$i * $gd}]
	    }
	    lappend ys $axes_length
	    
	    foreach y $ys {
		$canvas create line 0 -$y $plot_width -$y -fill white\
		    -width 2 -tag $visualid
	    }
	} else {
	    $canvas configure -bg white
	}
	
	
	# Axes 
	if {$showAxes} {
	    if {$showGuides} {
		set axesCol white
	    } else {
		set axesCol black
	    } 
	    foreach x $xAxes {
		$canvas create line $x 0 $x -$axes_length\
		    -fill $axesCol -width 2\
		    -tag $visualid
	    }
	}
	
	
	set id {}
	set i 0
	## Draw Lines
	
	foreach active [set ${model}::active]\
	    col $color\
	    glyph [set ${model}::glyphs]\
	    tag [set ${model}::tag]\
	    linewidth [set ${model}::linewidth] {
		
		if {$active} {
		    set coords {}
		    
		    foreach g $glyph x $xAxes {
			lappend coords $x [expr {-$direction * $g * $axes_length}]
		    }


		    if {$showArea} {
			lappend coords [lindex $xAxes end] 0\
			    [lindex $xAxes 0] 0
			
			lappend id [$canvas create polygon $coords\
					-fill $col -outline $col -width $linewidth\
					-tag [concat layer $visualid parallel item$i $tag]]
		    } else {
			
			lappend id [$canvas create line $coords\
					-fill $col -width $linewidth\
					-tag [list layer $visualid parallel item$i $tag]]
		    }
		    
		} else {
		    lappend id -1
		}
		incr i
	    }
	


	if {$showAxesLabels} {
	    
	    set labels [set ${model}::axesLabelsForSequence]
	    set y 20
	    
	    foreach label $labels x $xAxes {
		$canvas create text $x $y\
		    -text $label -anchor center -justify center -tag $visualid
	    }
	}




	
	$canvas move all $xmargin [expr {$axes_length+$ymargin_top}]
	
	if {$showLabels && $title ne ""} {
	    $canvas create text\
		[expr {$width/2.0}] [expr {$ymargin_top/2}]\
		-text $title -anchor c -font {Arial 18 bold}\
		-tag $visualid
	}
	
	my selectedAbove	
    }

}
