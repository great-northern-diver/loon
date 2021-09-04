
oo::class create loon::classes::ScalesGuidesVisual {

    superclass ::loon::classes::Visual


    variable soi showScales showGuides scale_color guidelinesColor

    constructor {args} {

	## states of interest
	set soi {panX panY zoomX zoomY deltaX deltaY swap\
		     vpx0 vpy0 vpx1 vpy1 swap}

	set showScales TRUE
	set showGuides TRUE

	set guidelinesColor white

	set scale_color black

	next {*}$args

    }


    method setColor {Color} {
	set scale_color $Color
	my redraw
    }

    method setShowGuides {value} {
	set showGuides $value
    }
    method setShowScales {value} {
	set showScales $value
    }
    method setGuidelinesColor {value} {
	set guidelinesColor $value
	my redraw
    }

    method updateScales {} {
	my redraw
    }
    method updateCoords {} {
	my redraw
    }

    method redraw {} {
	my variable canvas visualid map isVisible

	my clear

	if {!$isVisible && !$showScales && !$showGuides} {return}

	foreach var $soi {
	    set $var [$map get[string toupper $var 0]]
	}

	set plot_width [$map getPlotWidth]
	set plot_height [$map getPlotHeight]


	## Only show fixed Guidelines
	if {$showGuides && !$showScales} {
	    set guideid ""

	    set d $::loon::Options(fixed_guide_distance)

	    set x0 [expr {($plot_width % $d)/2}]
	    set y0 [expr {($plot_height % $d)/2}]

	    for {set x [expr {$vpx0 + $x0}]} {$x < $vpx1} {incr x $d} {
		$canvas create line $x $vpy0 $x $vpy1\
		    -fill $guidelinesColor\
		    -tag [list loon $visualid xscale guideline]\
		    -width 2
	    }

	    ## middle x guide line
	    for {set mx [expr {$vpx0 + $x0 + $d/2}]} {$mx < $vpx1} {incr mx $d} {
		$canvas create line $mx $vpy0 $mx $vpy1\
		    -fill $guidelinesColor\
		    -tag [list loon $visualid xscale middle guideline]\
		    -width 1
	    }

	    for {set y [expr {$vpy0 + $y0}]} {$y < $vpy1} {incr y $d} {
		$canvas create line $vpx0 $y $vpx1 $y\
		    -fill $guidelinesColor\
		    -tag [list loon $visualid yscale guideline]\
		    -width 2
	    }

	    ## middle y guide line
	    for {set my [expr {$vpy0 + $y0 + $d/2}]} {$my < $vpy1} {incr my $d} {
		$canvas create line $vpx0 $my $vpx1 $my\
		    -fill $guidelinesColor\
		    -tag [list loon $visualid yscale middle guideline]\
		    -width 1
	    }

	    $canvas lower guideline
	    return
	}

	set density $::loon::Options(ticks_density)
	set tick_length $::loon::Options(tick_length)


	# number of preferred labels
	set m_x [expr {int($density/100.0*$plot_width)}]
	if {$m_x <= 4} {
	   set m_x 5
	}
	set m_y [expr {int($density/100.0*$plot_height)}]
	if {$m_y <= 3} {
	   set m_y 4
	}

	if {$swap} {
	    set xfrom $panY
	    set xto [expr {$xfrom + $deltaY/$zoomY}]
	    set yfrom $panX
	    set yto [expr {$yfrom + $deltaX/$zoomX}]
	} else {
	    set xfrom $panX
	    set xto [expr {$xfrom + $deltaX/$zoomX}]
	    set yfrom $panY
	    set yto [expr {$yfrom + $deltaY/$zoomY}]
	}

	set x_ticks [::loon::scales::extended $xfrom $xto $m_x]
	set y_ticks [::loon::scales::extended $yfrom $yto $m_y]


	set font $::loon::Options(font-scales)

	## Draw x scales
	my variable canvas_height
	set x_ymove_ticks $vpy1
	set x_ymove_labels [expr {$x_ymove_ticks + $tick_length + 10}]

	set x_label_offset $vpx0
	set x_mul [expr {double($plot_width)/($xto-$xfrom)}]

	for {set i 0} {$i < [llength $x_ticks]} {incr i} {
	    set xtick [lindex $x_ticks $i]
	    set xpos [expr {$x_label_offset + ($xtick - $xfrom)*$x_mul}]
	    set oddOrEven [expr $i % 2]

	    ## odd index value draw axis, even do not
	    if {$oddOrEven == 1} {
	       set idet [$canvas create line 0 0 0 $tick_length -width 1\
			 -fill $scale_color\
			 -tag [list loon $visualid xscale tick]]
	       $canvas move $idet $xpos $x_ymove_ticks

	       set ide [$canvas create text 0 0\
		     -text [format "%.5g" $xtick]\
		     -anchor center\
		     -font $font\
		     -fill $scale_color\
		     -tag [list loon $visualid xscale label]]
	       $canvas move $ide $xpos $x_ymove_labels

	       if {$showGuides} {
		     $canvas create line $xpos $vpy0 $xpos $vpy1\
		       -fill $guidelinesColor\
		       -tag [list loon $visualid xscale guideline]\
		       -width 2
	       }
	    } else {
	   	  set idet [$canvas create line 0 0 0 0 -width 1\
			 -fill $scale_color\
			 -tag [list loon $visualid xscale tick]]
	      $canvas move $idet $xpos $x_ymove_ticks

	      if {$showGuides} {
		     $canvas create line $xpos $vpy0 $xpos $vpy1\
		       -fill $guidelinesColor\
		       -tag [list loon $visualid xscale guideline]\
		       -width 1
	      }
	    }
	}

	## Draw y scales

 	set y_xmove_ticks [expr {$vpx0 - $tick_length}]
	set y_xmove_labels [expr {$y_xmove_ticks - 8}]

	set y_label_offset $vpy1
	set y_mul [expr {double($plot_height)/($yto-$yfrom)}]

	for {set i 0} {$i < [llength $y_ticks]} {incr i} {
	    set ytick [lindex $y_ticks $i]
	    set ypos [expr {$y_label_offset - ($ytick - $yfrom)*$y_mul}]
	    set oddOrEven [expr $i % 2]

	    if {$oddOrEven != 1} {
	       set idet [$canvas create line 0 0 $tick_length 0 -width 1\
		     -fill $scale_color\
		     -tag [list loon $visualid yscale tick]]
	       $canvas move $idet $y_xmove_ticks $ypos

    	   set ide [$canvas create text 0 0\
			 -text [format "%.5g" $ytick]\
			 -anchor e\
			 -fill $scale_color\
			 -font $font\
			 -tag [list loon $visualid yscale label]]
	       $canvas move $ide $y_xmove_labels $ypos

	       if {$showGuides} {
	          lappend guideid [$canvas create line $vpx0 $ypos $vpx1 $ypos\
				     -fill $guidelinesColor\
				     -tag [list loon $visualid yscale guideline]\
				     -width 2]
	       }
	    } else {
	       set idet [$canvas create line 0 0 0 0 -width 1\
		     -fill $scale_color\
		     -tag [list loon $visualid yscale tick]]
	       $canvas move $idet $y_xmove_ticks $ypos

	       if {$showGuides} {
	          lappend guideid [$canvas create line $vpx0 $ypos $vpx1 $ypos\
				     -fill $guidelinesColor\
				     -tag [list loon $visualid yscale guideline]\
				     -width 1]
	      }
	    }
	}

	if {$showGuides} {
	    $canvas lower guideline
	}
    }

}
