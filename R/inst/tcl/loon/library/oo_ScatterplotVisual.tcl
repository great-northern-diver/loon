::oo::class create ::loon::classes::ScatterplotVisual {

    superclass ::loon::classes::LayerVisual

    variable ids isIdsL1 modelns n_var\
	xTemp_var yTemp_var\
	color_var size_var selected_var active_var\
	glyph_var glyphZoomSensitivity_var curGlyph\
	glyphVisuals glyphBinding glyphStateBindings\
	curX curY isCurXYinvalidated foreground_var

    constructor {Layerobj args} {

	set ids "noinit"
	set isIdsL1 "" ;# is ids length 1?

	## cache glyph, size and screen coordinates
	set curGlyph ""
	set curX ""
	set curY ""
	set isCurXYinvalidated TRUE

	set glyphVisuals [dict create]
	set glyphStateBindings [dict create]

	set modelns [set [info object namespace $Layerobj]::modelns]

	foreach state {n color size selected active xTemp yTemp\
			   glyph glyphZoomSensitivity foreground} {
	    set ${state}_var [uplevel #0 [list ${modelns}::my varname $state]]
	}


	next $Layerobj {*}$args


    }

    method MakeStateBinding {{withGlyphBinding TRUE}} {
	my variable statebinding
	set statebinding [${modelns}::my systembind state add all "[self] layerupdate %e"]
	if {$withGlyphBinding} {
	    set glyphBinding [${modelns}::my systembind glyph add all "[self] glyphupdate %g %e"]
	} else {
	    set glyphBinding ""
	}
    }


    destructor {
	my variable statebinding
	if {$statebinding ne ""} {
	    catch {my clear}
	    catch {${modelns}::my systembind state delete $statebinding}
	    set statebinding ""
	}
	if {$glyphBinding ne ""} {
	    catch {${modelns}::my systembind glyph delete $glyphBinding}
	}
	dict for {glyph binding} $glyphStateBindings {
	    catch {${modelns}::my glyph use $glyph systembind state delete $binding}
	}

	next
    }



    ## Called by state binding
    method layerupdate {Events} {
	## for speed
	if {$Events eq "selected" || $Events eq "color"} {
	    my recolor
	    return
	}

	## Copy events in a dictionary
	## for fast access of events
	set events [dict create]
	foreach e $Events {
	    dict set events $e 1
	}

	## Plot_View takes care of zoom and pan
	set nevents [llength $Events]
	set numHits 0
	foreach e {panX panY zoomX zoomY deltaX deltaY} {
	    if {[dict exists $events $e]} {incr numHits}
	}
	if {$numHits >= $nevents} {return}

	my LayerUpdateDict $events
    }

    method LayerUpdateDict {events} {

	if {[dict exists $events "n"] || [dict exists $events "glyph"]} {
	    my redraw
	} else {

	    if {[dict exists $events "active"]} {
		my updateActive
	    }

	    if {[dict exists $events "color"]||\
		    [dict exists $events "selected"]||\
		    [dict exists $events "foreground"]} {
		my recolor
	    }

	    if {[dict exists $events "tag"]} {
		my updateTags
	    }

	    if {[dict exists $events "size"]  || \
		    [dict exists $events "x"] || \
		    [dict exists $events "y"] || \
		    [dict exists $events "xTemp"] ||\
		    [dict exists $events "yTemp"]} {
		my updateCoords
	    }
	}

    }

    method glyphupdate {glyph event} {
	switch -- $event {
	    add {
		dict set glyphVisuals $glyph\
		    [my GlyphVisualFactory [${modelns}::my glyph getObject $glyph]]

		dict set glyphStateBindings $glyph\
		    [${modelns}::my glyph use $glyph systembind state add\
			 all "[self] glyphconfigured %g %e"]

	    }
	    delete {
		if {[dict exists $glyphStateBindings $glyph]} {

		    dict unset glyphStateBindings $glyph

		    # the glyph wont send any state events
		    # ${modelns}::my glyph use $glyph systembind state delete\
		    #	[dict get $glyphStateBindings $glyph]

		    [dict get $glyphVisuals $glyph] destroy
		    dict unset glyphVisuals $glyph
		}
	    }
	}

	## Should never be the case
	if {$glyph in $curGlyph} {
	    my redraw
	}

    }

    method glyphconfigured {glyph events} {
	my variable isVisible
	if {!$isVisible || [set $n_var] eq 0} {

	    return

	} elseif {$glyph in $curGlyph} {
	    my redraw
	}
    }


    method clear {} {
	my variable visualid canvas

	uplevel #0 [list $canvas delete $visualid]

	set ids "noinit"
	set curGlyph ""
    }

    ## the
    method redraw {{clear TRUE}} {
	my variable canvas visualid isVisible

	if {$clear || $ids ne "noinit"} {
	    my clear
	}

	if {$isVisible} { set state normal } else { set state hidden }

	if {[set $n_var] eq 0} {
	    set ids [uplevel #0 [list $canvas create oval 0 0 0 0\
				     -tag [list layer $visualid] -state hidden]]
	} else {

	    set ids {}
	    set isIdsL1 {}
	    set i 0
	    set curGlyph [set $glyph_var]

	    foreach glyph $curGlyph {
		switch -- $glyph {
		    circle -
		    ocircle -
		    ccircle {
			set ide [uplevel #0 [list $canvas create oval 0 0 0 0\
						 -state $state]]
		    }
		    square -
		    osquare -
		    csquare {
			set ide [uplevel #0 [list $canvas create rect 0 0 0 0\
						 -state $state]]
		    }
		    triangle -
		    otriangle -
		    ctriangle {
			set ide [uplevel #0 [list $canvas create polygon 0 0 0 0\
						 -state $state]]
		    }
		    diamond -
		    odiamond -
		    cdiamond {
			set ide [uplevel #0 [list $canvas create polygon 0 0 0 0\
						 -state $state]]
		    }
		    default {
			set ide [[dict get $glyphVisuals $glyph] draw $canvas $i]
		    }
		}
		incr i
		if {[llength $ide] eq 1} {
		    lappend isIdsL1 TRUE
		} else {
		    lappend isIdsL1 FALSE
		}
		lappend ids $ide
	    }

	    my updateTags
	    my updateCoords
	    my recolor
	    my updateActive
	}

	next ;## move layer into correct place
    }


    ## updateCoords updates the coordinates of all canvas items on the
    method updateCoords {} {
	my variable canvas isVisible

	if {!$isVisible || [set $n_var] eq 0} { return }

	if {[llength $ids] ne [set $n_var]} {
	    my redraw
	    return
	}

	my updateCurXY

	set i 0
	if {[llength $isIdsL1] eq 0} {
	    set isIdsL1 TRUE
	}

	if {[llength $curGlyph] eq 0} {
	    set curGlyph ccircle
	}
	foreach id $ids isL1 $isIdsL1\
	    x $curX y $curY \
	    glyph $curGlyph\
	    size [set $size_var] {

		switch -- $glyph {
		    circle -
		    ocircle -
		    ccircle {
			set r [::loon::map_circle_size $size]
			uplevel #0 [list $canvas coords $id -$r -$r $r $r]
		    }
		    square -
		    osquare -
		    csquare {
			set s [::loon::map_square_size $size]
			uplevel #0 [list $canvas coords $id -$s -$s $s $s]
		    }
		    triangle -
		    otriangle -
		    ctriangle {
			set s [::loon::map_triangle_size $size]
			set s1 [lindex $s 0]; set s2 [lindex $s 1]; set r [lindex $s 2]
			uplevel #0 [list $canvas coords $id -$s1 $s2 $s1 $s2 0 -$r]
		    }
		    diamond -
		    odiamond -
		    cdiamond {
			set s [::loon::map_diamond_size $size]
			uplevel #0 [list $canvas coords $id -$s 0 0 $s $s 0 0 -$s]
		    }
		    default {
			## not a primitive glyph
			[dict get $glyphVisuals $glyph] updateCoords\
			    $id $canvas $i $size
		    }
		}
		if {$isL1} {
		    uplevel #0 [list $canvas move $id $x $y]
		} else {
		    foreach ide $id {
			uplevel #0 [list $canvas move $ide $x $y]
		    }
		    ## too slow
		    ##uplevel #0 [list $canvas move "layer&&$visualid&&point$i" $x $y]
		}

		incr i
	    }
    }

    method updateCurXY {} {
	my variable map x_var y_var

	if {[llength [set $xTemp_var]] ne 0} {
	    set xvar $xTemp_var
	} else {
	    set xvar $x_var
	}
	if {[llength [set $yTemp_var]] ne 0} {
	    set yvar $yTemp_var
	} else {
	    set yvar $y_var
	}

	set sxycoords [$map mapDxy2Sxy [set $xvar] [set $yvar]]

	set curX [lindex $sxycoords 0]
	set curY [lindex $sxycoords 1]

	set isCurXYinvalidated FALSE
    }

    method updatedPan {args} {
	set isCurXYinvalidated TRUE
	next {*}$args
    }

    method updateZoomPan {oldPanX oldPanY oldZoomX oldZoomY} {
	my variable isVisible canvas map x_var y_var

	if {!$isVisible || [set $n_var] eq 0} { return }

	if {[llength [set $xTemp_var]] ne 0} {
	    set xvar $xTemp_var
	} else {
	    set xvar $x_var
	}
	if {[llength [set $yTemp_var]] ne 0} {
	    set yvar $yTemp_var
	} else {
	    set yvar $y_var
	}

	set diff [$map dPanZoom2dS [set $xvar] [set $yvar]\
		      $oldPanX $oldPanY $oldZoomX $oldZoomY]

	set i 0
	foreach id $ids isL1 $isIdsL1 dx [lindex $diff 0] dy [lindex $diff 1]\
	    needUpdateCoords [set $glyphZoomSensitivity_var] size [set $size_var]\
	    glyph $curGlyph {

		if {$needUpdateCoords} {

		    [dict get $glyphVisuals $glyph] updateCoords\
			$id $canvas $i $size

		    set xy [$map mapDxy2Sxy [lindex [set $xvar] $i] [lindex [set $yvar] $i]]

		    foreach ide $id {
			uplevel #0 [list $canvas move $ide {*}$xy]
		    }


		} else {
		    if {$isL1} {
			uplevel #0 [list $canvas move $id $dx $dy]
		    } else {
			foreach ide $id {
			    uplevel #0 [list $canvas move $ide $dx $dy]
			}
		    }
		}
		incr i
	    }
	set isCurXYinvalidated TRUE
    }




    method GlyphVisualFactory {GlyphObject} {
	my variable map x_var y_var

	set class [info object class $GlyphObject]

	if {[catch {set visualObj [${class}Visual new $GlyphObject $map $x_var $y_var]}]} {
	    error "Glyph visual for \"[string range $class 17 end]\" not known."
	} else {
	    return $visualObj
	}
    }


    method recolor {{highlightSelected TRUE}} {
	my variable isVisible canvas

	if {!$isVisible || [set $n_var] eq 0} { return }

	set foreground [set $foreground_var]

	set selColor $::loon::Options(select-color)

	set cols [set $color_var]
	if {$highlightSelected} {
	    set i 0
	    foreach s [set $selected_var] {
		if {$s} {
		    lset cols $i $selColor
		}
		incr i
	    }
	}

	set i 0
	foreach id $ids glyph $curGlyph color $cols {

	    switch -- $glyph {
		circle -
		square -
		triangle -
		diamond {
		    uplevel #0 [list $canvas itemconfigure $id\
				    -fill $color -outline $color]
		}
		ocircle -
		osquare -
		otriangle -
		odiamond {
		    uplevel #0 [list $canvas itemconfigure $id\
				    -fill "" -outline $color]
		}
		ccircle -
		csquare -
		ctriangle -
		cdiamond {
		    uplevel #0 [list $canvas itemconfigure $id\
				    -fill $color -outline $foreground]
		}
		default {
		    [dict get $glyphVisuals $glyph] recolor\
			$id $canvas $i $color
		}
	    }
	    incr i
	}
	my raiseSelectedToTop
    }

    method updateActive {} {
	my variable isVisible canvas

	if {!$isVisible || [set $n_var] eq 0} { return }

	foreach id $ids active [set $active_var] {
	    if {$active} {set state normal} else {set state hidden}

	    foreach ide $id {
		uplevel #0 [list $canvas itemconfigure $ide -state $state]
	    }

	}
    }

    method updateTags {} {
	my variable canvas isVisible visualid tag_var

	if {!$isVisible || [set $n_var] eq 0} { return }

	set i 0
	foreach id $ids t [set $tag_var] {

	    set ttags [list layer $visualid point item$i $t]

	    if {[llength $id] eq 1} {
		uplevel #0 [list $canvas itemconfigure $id -tag $ttags]
	    } else {
		foreach ide $id {
		    uplevel #0 [list $canvas itemconfigure $ide -tag $ttags]
		}
	    }
	    incr i
	}
    }


    # ## Too slow
    method raiseSelectedToTop {} {
	my variable isVisible visualid canvas

      	if {$isVisible && [set $n_var] ne 0} {
	    foreach id $ids sel [set $selected_var] l1 $isIdsL1 {
      		if {$sel} {
		    if {$l1} {
			uplevel #0 [list $canvas raise $id $visualid]
		    } else {
			foreach ide $id {
			    uplevel #0 [list $canvas raise $ide $visualid]
			}
		    }
		}
      	    }
      	}
    }

    method setVisibility {value} {
	my variable isVisible visualid canvas
	set isVisible $value

	## make sure that only active points get shown
	if {$value} {
	    my redraw
	} else {
	    set tmp [uplevel #0 [list $canvas create oval 0 0 0 0 -state hidden]]
	    uplevel #0 [list $canvas lower $tmp $visualid]
	    my clear
	    set ids $tmp
	}
    }


    method indexWithCurrentTag {} {
	my variable canvas visualid

	set tags [$canvas gettags current]
	if {[lindex $tags 0] eq "layer" &&\
		[lindex $tags 1] eq $visualid &&\
		[lindex $tags 2] eq "point"} {
	    return [string range [lindex $tags 3] 4 end]
	} else {
	    return {}
	}

    }

    method indecesWithinRect {x0 y0 x1 y1} {
	my variable canvas map

	set vpx0 [$map getVpx0]; set vpx1 [$map getVpx1]
	set vpy0 [$map getVpy0]; set vpy1 [$map getVpy1]
	if {$x0 < $x1} {
	    if {$x0 > $vpx0} { set x0rect $x0 } else { set x0rect $vpx0 }
	    if {$x1 < $vpx1} { set x1rect $x1 } else { set x1rect $vpx1 }
	} else {
	    if {$x1 > $vpx0} { set x0rect $x1 } else { set x0rect $vpx0 }
	    if {$x0 < $vpx1} { set x1rect $x0 } else { set x1rect $vpx1 }
	}
	if {$y0 < $y1} {
	    if {$y0 > $vpy0} { set y0rect $y0 } else { set y0rect $vpy0 }
	    if {$y1 < $vpy1} { set y1rect $y1 } else { set y1rect $vpy1 }
	} else {
	    if {$y1 > $vpy0} { set y0rect $y1 } else { set y0rect $vpy0 }
	    if {$y0 < $vpy1} { set y1rect $y0 } else { set y1rect $vpy1 }
	}
	set inRect [$canvas find overlapping $x0rect $y0rect $x1rect $y1rect]

	return [my ItemIndicesForCanvasIds $inRect]
    }

    method ItemIndicesForCanvasIds {cids} {
	my variable canvas visualid

	if {[llength $ids] eq 0} {return {}}

	set indices {}

	foreach id $cids {
	    set tags [$canvas gettags $id]
	    if {[lindex $tags 0] eq "layer" &&\
		    [lindex $tags 1] eq $visualid &&\
		    [lindex $tags 2] eq "point"} {
		lappend indices [string range [lindex $tags 3] 4 end]
	    }
	}
	## certain glyphs migth have more than one
	return [lsort -unique $indices]
    }

}

