oo::class create loon::classes::ParallelCoordinatesVisual {

    superclass ::::loon::classes::SerialaxesAbstractVisual


    method redraw {} {
	my variable canvas ids visualid model

	if {$model eq ""} {return}

	if {$ids ne "noinit"} {
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

    # As the window first created, the size is smaller than the required minimum
    # size. Warning "Window is too small" will be executed.
    # Set such logical check can help us avoid the initial warning.
    if {$plot_width > -150} {
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
    } else {
      ## initial run
      my clear
	  return
    }

	foreach state {showGuides showAxes showAxesLabels andrews showLabels showArea color andrewsSeriesLength} {
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

	set ids {}
	set i 0
	## Draw Lines

	if {$andrews} {

      # x position
      set minx [::loon::listfns::min $xAxes]
      set maxx [::loon::listfns::max $xAxes]
      set newx [::loon::listfns::lseq2 $minx $maxx $andrewsSeriesLength]

      # find the minimum and maximum value of the fourier tranformation
      set ming 0
      set maxg 0

      set newg {}

      foreach glyph [set ${model}::glyphs] {

        set newglyph [::loon::listfns::fourierTrans $glyph $andrewsSeriesLength]
        lappend newg $newglyph

        if {$ming eq "" || $maxg eq ""} {
          set ming [::loon::listfns::min $newglyph]
          set maxg [::loon::listfns::max $newglyph]
        } else {
          set ming [::loon::listfns::min [lappend newglyph $ming]]
          set maxg [::loon::listfns::max [lappend newglyph $maxg]]
        }
      }

      foreach active [set ${model}::active]\
	    col $color\
	    newglyph $newg\
	    tag [set ${model}::tag]\
	    linewidth [set ${model}::linewidth] {

		if {$active} {

		    set coords {}
		    set newglyph01 [::loon::listfns::scale01 $newglyph $ming $maxg]

            foreach g $newglyph01 x $newx {
			  lappend coords $x [expr {-$direction * $g * $axes_length}]
		    }

		    if {$showArea} {
			lappend coords [lindex $newx end] 0\
			    [lindex $newx 0] 0

			lappend ids [$canvas create polygon $coords\
					-fill $col -outline $col -width $linewidth\
					-tag [concat layer $visualid parallel item$i $tag]]
		    } else {

			lappend ids [$canvas create line $coords\
					-fill $col -width $linewidth\
					-tag [list layer $visualid parallel item$i $tag]]
		    }

		} else {
		    lappend ids -1
		}
		incr i
	  }

	} else {

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

			lappend ids [$canvas create polygon $coords\
					-fill $col -outline $col -width $linewidth\
					-tag [concat layer $visualid parallel item$i $tag]]
		    } else {

			lappend ids [$canvas create line $coords\
					-fill $col -width $linewidth\
					-tag [list layer $visualid parallel item$i $tag]]
		    }

		} else {
		    lappend ids -1
		}
		incr i
	    }
	}



	if {$showAxesLabels} {

        if {$andrews} {
          set pi 3.1415926535897931
          set ts [::loon::listfns::lseq2  -$pi $pi $p]
	      set labels [::loon::listfns::round2 $ts]

        } else {
          set labels [set ${model}::axesLabelsForSequence]
        }

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
