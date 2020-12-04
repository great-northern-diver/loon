
oo::class create loon::classes::StarglyphsStackedVisual {

    superclass ::loon::classes::SerialaxesAbstractVisual

    variable angles xdirs ydirs pi modelns

    constructor {args}  {

	## hash angles and directions
	set angles {}
	set xdirs {}
	set ydirs {}
	set pi 3.1415926535897931

	next {*}$args

    }



    method redraw {} {
	my variable canvas visualid ids modelns

	if {$ids ne "noinit"} {
	    my clear
	}

	if {[llength [set ${modelns}::glyphs]] eq 0} {
	    return
	}
	set width [winfo width $canvas]
	set height [winfo height $canvas]


	set font "Arial 14"
	set margin 200

	set radius [expr {(min($width,$height)-$margin)/2.0}]

	if {$radius < 20} {

	    my clear
	    set ids [$canvas create text [expr {$width/2.0}] [expr {$height/2.0}]\
			-anchor center\
			-justify center\
			-text "Window is\ntoo small"\
			-tag $visualid\
			-font $font]
	    return
	}

	set p [llength [set ${modelns}::sequence]]

	## angles and directions
	if {[llength $angles] ne $p} {
	    set angles {}; set xdirs {}; set ydirs {}
	    if {$p > 0} {
		for {set i 0} {$i < $p} {incr i} {
		    set angle [expr {2*$pi*(1-double($i)/$p)}]
		    lappend angles $angle
		    lappend xdirs [expr {double(cos($angle))}]
		    lappend ydirs [expr {double(sin($angle))}]
		}
	    }
	}


	## Get widget options from modelns

	foreach state {showGuides showAxes showAxesLabels showLabels showArea color} {
	    set $state [set ${modelns}::$state]
	}

	set sel_color $::loon::Options(select-color)

	set i 0
	foreach s [set ${modelns}::selected] {
	    if {$s} {
		lset color $i $sel_color
	    }
	    incr i
	}


	# Guides
	if {$showGuides} {
	    $canvas configure -bg $::loon::Options(canvas_bg_guides)

	    set minGuideDist $::loon::Options(fixed_guide_distance)
	    set ng [expr {int($radius/$minGuideDist)}]
	    set gd [expr {double($radius/$ng)}]
	    incr ng -1

	    set rgs {}
	    for {set i 1} {$i <= $ng} {incr i} {
		lappend rgs [expr {$i * $gd}]
	    }
	    lappend rgs $radius

	    foreach rg $rgs {
		$canvas create oval -$rg -$rg $rg $rg -fill "" -outline white\
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
	    set i 0
	    foreach xdir $xdirs ydir $ydirs {
		$canvas create line 0 0\
		    [expr {$radius * $xdir}] [expr {$radius * $ydir}]\
		    -fill $axesCol -width 2\
		    -tag [list $visualid loon_axis axis$i]
		incr i
	    }
	}

	set direction 1

	set ids {}
	set i 0
	## Draw Glyphs

	foreach active [set ${modelns}::active]\
	    col $color\
	    glyph [set ${modelns}::glyphs]\
	    tag [set ${modelns}::tag]\
	    linewidth [set ${modelns}::linewidth] {



		#puts "--- $i"
		#foreach v {glyph xdirs ydirs direction linewidth} {
		#    puts [format "%s: %s" $v [llength [set $v]]]
		#}

		if {$active} {
		    set coords {}
		    foreach g $glyph xdir $xdirs ydir $ydirs {
			if {$direction > 0} {
			    lappend coords [expr {$radius * $g * $xdir}]
			    lappend coords [expr {$radius * $g * $ydir}]
			} else {
			    lappend coords [expr {$radius * (1 - $g) * $xdir}]
			    lappend coords [expr {$radius * (1 - $g) * $ydir}]
			}
		    }
		    if {$showArea} {
			lappend ids [$canvas create polygon $coords\
					-fill $col -outline $col -width $linewidth\
					-tag [concat layer $visualid radial item$i $tag]]
		    } else {
			lappend coords {*}[lrange $coords 0 1]

			lappend ids [$canvas create line $coords\
					-fill $col -width $linewidth\
					-tag [concat layer $visualid radial item$i $tag]]
		    }
		} else {
		    lappend ids -1
		}
		incr i
	    }


	## Show Axes Labels
	if {$showAxesLabels} {

	    set labels [set ${modelns}::axesLabelsForSequence]

	    foreach label $labels angle $angles xdir $xdirs ydir $ydirs {
		if {$angle > 0 && $angle < 1.570796} {
		    ## in +-
		    set anchor "nw"
		} elseif {$angle >= 1.570796 && $angle < $pi} {
		    ## in --
		    set anchor "ne"
		} elseif {$angle >= $pi && $angle < 4.712389} {
		    ## in -+
		    set anchor "se"
		} else {
		    ## in ++
		    set anchor "sw"
		}

		$canvas create text\
		    [expr {$xdir*($radius+25)}]\
		    [expr {$ydir*($radius+25)}]\
		    -text $label -anchor $anchor -justify center -tag $visualid
	    }
	}

	$canvas move all [expr {$width/2.0}] [expr {$height/2.0}]

	if {$showLabels} {
	    $canvas create text\
		[expr {$width/2.0}] 20\
		-text [set ${modelns}::title] -anchor c -font {Arial 18 bold}\
		-tag $visualid
	}

	$canvas move all 0 20

	my selectedAbove

    }



}
