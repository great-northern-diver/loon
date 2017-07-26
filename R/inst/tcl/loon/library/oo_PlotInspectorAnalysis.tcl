
oo::class create loon::classes::PlotInspectorAnalysis {
    
    superclass ::loon::classes::Inspector2
    
    variable plotWidget\
	selectByColorMenu\
	linkingGroupWidget\
	currentGlyphSelected

    constructor {path} {
	set plotWidget ""
	
	next $path
	
    }
    

    method swapAxes {var} {
	if {$plotWidget ne ""} {
	    uplevel #0 [list $plotWidget configure -swapAxes [set ::$var]]
	}
    }

    method showScales {var} {
	if {$plotWidget ne ""} {
	    uplevel #0 [list $plotWidget configure -showScales [set ::$var]]
	}
    }

    method showGuides {var} {
	if {$plotWidget ne ""} {
	    uplevel #0 [list $plotWidget configure -showGuides [set ::$var]]
	}
    }
    
    method showLabels {var} {
	if {$plotWidget ne ""} {
	    uplevel #0 [list $plotWidget configure -showLabels [set ::$var]]
	}
    }

    method showItemLabels {var} {
	if {$plotWidget ne ""} {
	    uplevel #0 [list $plotWidget configure -showItemLabels [set ::$var]]
	}
    }



    

    method selectStatic {which} {
	if {$plotWidget ne ""} {

	    switch -- $which {
		all {
		    uplevel #0 [list $plotWidget configure -selected TRUE -which all]
		}
		none {
		    uplevel #0 [list $plotWidget configure -selected FALSE -which all]
		}
		invert {
		    set selected\
			[::loon::listfns::booleanNot [::$plotWidget cget -selected]]
		    uplevel #0 [list $plotWidget configure -selected $selected]
		}
		default {
		    error "invalid which $which"
		}		
	    }
	}	
    }

    method selectDynamic {var} {
	if {$plotWidget ne ""} {
	    uplevel #0 [list $plotWidget configure -selectionLogic [set ::${var}]]
	}
    }

    method selectBy {var} {
	if {$plotWidget ne ""} {
	    uplevel #0 [list $plotWidget configure -selectBy [set ::${var}]]
	}
    }
    
    method selectByColor {add} {
	
	if {$plotWidget eq ""} {
	    return
	}
	
	set canvas ${selectByColorMenu}.canvas
	set i [lindex [$canvas gettags current] 2]
	set sel_color [$canvas itemcget "color && inner && $i" -fill]
	
	set color [$plotWidget cget -color]
	
	if {$add} {
	    set selected [$plotWidget cget -selected]
	} else {
	    set selected [lrepeat [llength $color] FALSE]
	}

	set i 0
	foreach c $color {
	    if {$c eq $sel_color} {
		lset selected $i TRUE
	    } 
	    incr i
	}

	$plotWidget configure -selected $selected       
    }

    method modifyColor {canvas buttonState} {
	if {$plotWidget eq ""} {
	    return
	}
	
	set i [lindex [$canvas gettags current] 2]
	set col [$canvas itemcget "color && inner && $i" -fill]
	
	if {$buttonState eq "press"} {
	    ## violate encapsulation
	    #::loon::plot::Recolor $plotWidget $col 
	} elseif {$buttonState eq "release"} {
	    uplevel #0 [list $plotWidget configure -color $col -which "selected"]
	}
    }
    
    method changeActive {active} {
	if {$plotWidget eq ""} {
	    return
	}
	
	if {$active eq "deactivate"} {
	    uplevel #0 [list $plotWidget configure -active 0 -which selected]
	} elseif {$active eq "reactivate"} {
	    uplevel #0 [list $plotWidget configure -active 1 -which all]
	}
    }

    method movePoints {how} {
	if {$plotWidget ne ""} { 
	    set args {}
	    if {$how eq "jitter"} {
		# jitter factor
		lappend args 1 0
	    }
	    $plotWidget move $how selected {*}$args
	}
    }
    
    
    method displayGlyphSelection {optionMenu} {
	if {$plotWidget eq ""} {
	    return
	}
	
	${optionMenu}.menu delete 0 end
	
	
	
	## Violate encapsulation!
	## get names
	set hasGlyphs FALSE
	foreach id [$plotWidget glyph ids] {

	    set glabel [$plotWidget glyph getLabel $id]		 
	    if {$glabel eq ""} {
		set pdlabel $id
	    } else {
		set pdlabel [format "%s, %s" $glabel $id]
	    }
	    
	    ${optionMenu}.menu add radiobutton\
		-variable ${optionMenu}Var\
		-label $pdlabel
	    
	    set hasGlyphs TRUE
	}
	if {!$hasGlyphs} {
	    ${optionMenu}.menu add radiobutton\
		-label "---" -variable ${optionMenu}Var
	}
    }

    method changeGlyphs {var} {
	if {$plotWidget eq ""} {
	    return
	}
	
	set glyph [set ::$var]
	if {$glyph ne "---"} {
	    set glyph [lindex [regexp -inline "^.*(glyph\\d+)$" $glyph] 1]
	    if {[llength $glyph] eq 0} {
		::loon::warning "glyph \"[set ::$var]\" not found."
	    }
	    uplevel #0 [list $plotWidget configure\
			    -glyph $glyph -which selected]
	}	
    }
    
    method changeGlyphsButton {glyph} {
	if {$plotWidget eq ""} {
	    return
	}
	uplevel #0 [list $plotWidget configure -glyph $glyph -which selected]	
    }
    

    method changeSize {how} {
	if {$plotWidget ne ""} {

	    set selected [$plotWidget cget -selected]

	    if {![::loon::listfns::any $selected]} {
		return
	    }
	    
	    set size {} 
	    set oldSize [::loon::listfns::subsetLogical\
			     [$plotWidget cget -size]\
			     $selected]
	    
	    set n [llength $oldSize]
	    switch -- $how {
		abs+1 {
		    set min [::loon::listfns::min $oldSize]
		    set size [lrepeat $n [expr {$min + 1}]]
		}
		abs-1 {
		    set min [::loon::listfns::min $oldSize]
		    if {$min < 0} {
			bell
			return
		    } else {
			set size [lrepeat $n [expr {$min - 1}]]
		    }
		}
		rel+1 {
		    foreach e $oldSize {
			lappend size [expr {$e+1}]
		    }		    
		}
		rel-1 {
		    set min [::loon::listfns::min $oldSize]
		    if {$min < 0} {
			bell
			return
		    } else {
			foreach e $oldSize {
			    lappend size [expr {$e-1}]
			}	
		    }
		}
	    }	 
	    uplevel #0 [list $plotWidget configure -size $size -which selected]
	}	
    }

    method scaleTo {what} {
	if {$plotWidget ne ""} {
	    $plotWidget scaleto $what
	}	
    }
    
    method Make {} {
	my variable path	
	frame $path -class LoonPlotInspectorAnalysis
	

	## Pack all the controls
	pack [labelframe ${path}.plot -text "Plot" -padx 2 -pady 2]\
	    [labelframe ${path}.select -text "Select" -padx 2 -pady 2]\
	    [labelframe ${path}.modify -text "Modify" -padx 2 -pady 2]\
	    -side top -fill x -padx 5 -pady 2.5 -anchor w
	


	## Plot Options
	set opt [frame ${path}.plot.show]
	pack $opt -side top -fill x -expand TRUE
	
	label ${opt}.swapLabel -text "axes:"
	checkbutton ${opt}.swapCB -text "swap"\
	    -variable ${opt}.swapVar\
	    -onvalue 1 -offvalue 0\
	    -command [list [self namespace]::my swapAxes ${opt}.swapVar]
	
	checkbutton ${opt}.showScalesCB -text "scales"\
	    -variable ${opt}.showScalesVar\
	    -onvalue 1 -offvalue 0\
	    -command [list [self namespace]::my showScales\
			  ${opt}.showScalesVar]
	checkbutton ${opt}.showGuidesCB -text "guides"\
	    -variable ${opt}.showGuidesVar\
	    -onvalue 1 -offvalue 0\
	    -command [list [self namespace]::my showGuides\
			  ${opt}.showGuidesVar]
	checkbutton ${opt}.showLabelsCB -text "labels"\
	    -variable ${opt}.showLabelsVar\
	    -onvalue 1 -offvalue 0\
	    -command [list [self namespace]::my showLabels\
			  ${opt}.showLabelsVar]
	
#	::loon::loonlabel_for_widget ${opt}.showLabelsCB\
#	    "show axes labels and title"

	label ${opt}.glyphsLabel -text "glyphs:"
	
	checkbutton ${opt}.itemLabelsCB -text "itemLabels"\
	    -variable ${opt}.showItemLabelsVar\
	    -onvalue 1 -offvalue 0\
	    -command [list [self namespace]::my showItemLabels\
			  ${opt}.showItemLabelsVar]

	
	## scaleto 
	label ${opt}.scaletoLabel -text "scale to:"
	frame ${opt}.scaleto
	
	pack [button ${opt}.scaleto.selected -text "selected"\
		  -command [list [self namespace]::my scaleTo selected]]\
	    [button ${opt}.scaleto.points -text "plot"\
		 -command [list [self namespace]::my scaleTo plot]]\
	    [button ${opt}.scaleto.world -text "world"\
		 -command [list [self namespace]::my scaleTo world]]\
	    -side left

	
	
	## Linking	

	label ${opt}.linkingGroupLabel -text "linking group:"
	set linkingGroupWidget [::loon::linkingGroupWidget ${opt}.linkingGroupWidget]
	
	grid ${opt}.swapLabel -row 0 -column 0 -sticky w
	grid ${opt}.swapCB -row 0 -column 1 -sticky w
	grid ${opt}.showScalesCB -row 1 -column 1 -sticky w
	grid ${opt}.showGuidesCB -row 1 -column 2 -sticky w
	grid ${opt}.showLabelsCB -row 0 -column 2 -sticky w
	grid ${opt}.glyphsLabel -row 3 -column 0 -sticky w
	
	#grid ${opt}.glyphsTransparentCB -row 3 -column 1 -sticky w
	
	grid ${opt}.itemLabelsCB -row 3 -column 1 -columnspan 2 -sticky w

	grid ${opt}.linkingGroupLabel -row 4 -column 0 -sticky w
	grid $linkingGroupWidget -row 4 -column 1 -sticky we -columnspan 3 -pady 5
	
	grid ${opt}.scaletoLabel -row 5 -column 0 -sticky w
	grid ${opt}.scaleto -row 5 -column 1 -sticky w -columnspan 2

	grid columnconfigure ${opt} 3 -weight 1
	
	grid columnconfigure ${opt} 0 -pad 5
	

	# Select
	set sel [frame ${path}.select.controls]
	pack $sel -side top -fill x
	
	label ${sel}.staticLabel -text "static:"
	frame ${sel}.static
	pack [button ${sel}.static.all -text "all"\
		  -command [list [self namespace]::my selectStatic all]]\
	    [button ${sel}.static.none -text "none"\
		 -command [list [self namespace]::my selectStatic none]]\
	    [button ${sel}.static.invert -text "invert"\
		 -command [list [self namespace]::my selectStatic invert]]\
	    -side left -anchor w
	
	label  ${sel}.dynamicLabel -text "dynamic:"
	frame ${sel}.dynamic

	set dynamicVar ${sel}.dynamicVar
	set cmdList [list [self namespace]::my selectDynamic $dynamicVar]
	pack [radiobutton ${sel}.dynamic.select -text "select"\
		  -variable $dynamicVar\
		  -value select -command $cmdList]\
	    [radiobutton ${sel}.dynamic.deselect -text "deselect"\
		 -variable $dynamicVar\
		 -value deselect -command $cmdList]\
	    [radiobutton ${sel}.dynamic.invert -text "invert"\
		 -variable $dynamicVar\
		 -value invert -command $cmdList]\
	    -side left
	
	label ${sel}.byLabel -text "by:"
	frame ${sel}.by
	
	set cmdList [list [self namespace]::my selectBy ${sel}.byVar]
	pack [radiobutton ${sel}.by.sweeping -text "sweeping"\
		  -variable ${sel}.byVar\
		  -value sweeping -command $cmdList]\
	    [radiobutton ${sel}.by.brushing -text "brushing"\
		 -variable ${sel}.byVar\
		 -value brushing -command $cmdList]\
	    -side left
	
	grid ${sel}.staticLabel -row 1 -column 0 -sticky w
	grid ${sel}.static -row 1 -column 1 -sticky w
	
	grid ${sel}.dynamicLabel -row 2 -column 0 -sticky w
	grid ${sel}.dynamic -row 2 -column 1 -sticky w
	
	grid ${sel}.byLabel -row 3 -column 0 -sticky w
	grid ${sel}.by -row 3 -column 1 -sticky w
	
	grid columnconfigure ${sel} 0 -pad 5

	pack [label ${path}.select.byColorLabel -text "by color:"]\
	    -side top -anchor w
	
	set selectByColorMenu\
	    [::loon::colormenu ${path}.select.byColor -hasAddColor 0]
	
	pack $selectByColorMenu -fill x
	
	${selectByColorMenu}.canvas bind "color" <Button-1>\
	    [list [self namespace]::my selectByColor FALSE]
	${selectByColorMenu}.canvas bind "color" <Shift-Button-1>\
	    [list [self namespace]::my selectByColor TRUE]
	
	# Modify
	set mod ${path}.modify
	
	pack [::loon::colormenu ${mod}.colors] -fill x 
	
	${mod}.colors.canvas bind "color" <ButtonPress-1>\
	    [list [self namespace]::my modifyColor %W "press"]

	${mod}.colors.canvas bind "color" <ButtonRelease-1>\
	    [list [self namespace]::my modifyColor %W "release"]

	
	pack [frame ${mod}.active] -side top -anchor w
	
	pack [button ${mod}.active.deactivate -text "deactivate"\
		  -command [list [self namespace]::my changeActive deactivate]]\
	    [button ${mod}.active.reactivate -text "reactivate"\
		 -command [list [self namespace]::my changeActive reactivate]]\
	    -side left
	
	set modf [frame ${path}.modify.frame]
	pack $modf -side top -fill x
	
	label ${modf}.moveLabel -text "move:"
	frame ${modf}.move

	set imgAlignV [image create bitmap\
			   -file [file join $::env(LOON_LIBRARY)\
				      images align_v.xbm]]
	set imgAlignH [image create bitmap\
			   -file [file join $::env(LOON_LIBRARY)\
				      images align_h.xbm]]
	set imgDistV [image create bitmap\
			  -file [file join $::env(LOON_LIBRARY)\
				     images distribute_v.xbm]]
	set imgDistH [image create bitmap\
			  -file [file join $::env(LOON_LIBRARY)\
				     images distribute_h.xbm]]
	set imgGrid [image create bitmap\
			 -file [file join $::env(LOON_LIBRARY)\
				    images distribute_grid.xbm]]
	set imgJitter [image create bitmap\
			 -file [file join $::env(LOON_LIBRARY)\
				    images jitter.xbm]]
	
	set imgReset [image create bitmap\
			  -file [file join $::env(LOON_LIBRARY)\
				     images reset.xbm]]
	
	pack [button ${modf}.move.align_v\
		  -image $imgAlignV\
		  -command [list [self namespace]::my movePoints valign]]\
	    [button ${modf}.move.align_h\
		 -image $imgAlignH\
		 -command [list [self namespace]::my movePoints halign]]\
	    [button ${modf}.move.distribute_v\
		 -image $imgDistV\
		 -command [list [self namespace]::my movePoints vdist]]\
	    [button ${modf}.move.distribute_h\
		 -image $imgDistH\
		 -command [list [self namespace]::my movePoints hdist]]\
	    [button ${modf}.move.distribute_grid\
		 -image $imgGrid\
		 -command [list [self namespace]::my movePoints grid]]\
	    [button ${modf}.move.jitter\
		 -image $imgJitter\
		 -command [list [self namespace]::my movePoints jitter]]\
	    [button ${modf}.move.reset\
		 -image $imgReset\
		 -command [list [self namespace]::my movePoints reset]]\
	    -side left
	
	label ${modf}.glyphLabel -text "glyph:"

	frame ${modf}.glyph
	frame ${modf}.glyphb


	## {circle square triangle diamond ocircle osquare otriangle odiamond}
	foreach g {circle square triangle\
		       ocircle osquare otriangle\
		       ccircle csquare ctriangle} {
	    pack [button ${modf}.glyphb.${g}\
		      -image [image create bitmap\
				  -file [file join $::env(LOON_LIBRARY)\
					     images ${g}.xbm]]\
		      -command "[self namespace]::my changeGlyphsButton $g"]\
		-side left		      
	}

	set currentGlyphSelected ${modf}.glyph.optionVar
	
	tk_optionMenu ${modf}.glyph.option\
	    ${modf}.glyph.optionVar "---"
	
	bind ${modf}.glyph.option <ButtonPress-1>\
	    [list [self namespace]::my displayGlyphSelection %W]

	${modf}.glyph.option configure -width 10 -anchor w
	
	button ${modf}.glyph.set -text "set"\
	    -command [list [self namespace]::my changeGlyphs\
			  ${modf}.glyph.optionVar]
	
	pack ${modf}.glyph.set\
	    ${modf}.glyph.option\
	    -side right
	
	label ${modf}.sizeLabel -text "size:"
	set mods [frame ${modf}.size]
	
	set imgPlus [image create bitmap\
			 -file [file join $::env(LOON_LIBRARY)\
				    images plus.xbm]]
	set imgMinus [image create bitmap\
			  -file [file join $::env(LOON_LIBRARY)\
				     images minus.xbm]]
	
	label  ${mods}.absLabel -text "abs:"
	button ${mods}.absm1 -image $imgMinus\
	    -command [list [self namespace]::my changeSize "abs-1"]
	button ${mods}.absp1 -image $imgPlus\
	    -command [list [self namespace]::my changeSize "abs+1"]
	
	label  ${mods}.relLabel -text "rel:"
	button ${mods}.relm1 -image $imgMinus\
	    -command [list [self namespace]::my changeSize "rel-1"]
	button ${mods}.relp1 -image $imgPlus\
	    -command [list [self namespace]::my changeSize "rel+1"]

	pack ${mods}.absLabel\
	    ${mods}.absm1\
	    ${mods}.absp1\
	    -side left
	
	pack ${mods}.relLabel\
	    ${mods}.relm1\
	    ${mods}.relp1\
	    -side left -padx {5 0}

	

	grid ${modf}.moveLabel -row 0 -column 0 -sticky w
	grid ${modf}.move -row 0 -column 1 -sticky w
	
	grid ${modf}.glyphLabel -row 1 -column 0 -sticky w
	grid ${modf}.glyphb -row 1 -column 1 -sticky w
	grid ${modf}.glyph -row 2 -column 1 -sticky w
	
	grid ${modf}.sizeLabel -row 3 -column 0 -sticky w
	grid ${modf}.size -row 3 -column 1 -sticky w

	grid columnconfigure ${modf} 0 -pad 5
    }


    method CheckNewActivewidget {widget} {
	if {![info object isa typeof $widget\
		  "::loon::classes::Widget"]} {
	    error "$widget is not a Scatterplot Widget."
	}
    }
    
    
    method RegisterActivewidget {} {
	my variable activewidget
	
	next
	## update inspector
	set plotWidget $activewidget
	if {$activewidget ne ""} {
	    my ActivewidgetEvents\
		swapAxes showLabels showScales showGuides\
		selectionLogic selectBy linkingGroup\
		color active showItemLabels
	    
	    set glyphs [$activewidget glyph ids]
	    if {[llength $glyphs] eq 0} {
		uplevel #0 set $currentGlyphSelected "---"
	    } else {
		set id [lindex $glyphs 0]
		set glabel [$activewidget glyph getLabel $id]		 
		if {$glabel eq ""} {
		    set pdlabel $id
		} else {
		    set pdlabel [format "%s, %s" $glabel $id]
		}		
		uplevel #0 [list set $currentGlyphSelected $pdlabel]
	    }
	}
	
	$linkingGroupWidget configure -activewidget $activewidget
    }
    
    
    
    method ActivewidgetEvents {args} {
	## args == events
	my variable path
	
	## TODO: fix, sometimes the events come nested
	if {[llength $args] eq 1} {
	    set args {*}$args
	}

	if {"swapAxes" in $args} {
	    uplevel #0 [list set ${path}.plot.show.swapVar\
			    [::loon::listfns::booleanAs01\
				 [$plotWidget cget -swapAxes]]]
	}
	
	if {"showLabels" in $args} {
	    uplevel #0 [list set ${path}.plot.show.showLabelsVar\
			    [::loon::listfns::booleanAs01\
				 [$plotWidget cget -showLabels]]]	    
	}

	
	if {"showScales" in $args} {
	    uplevel #0 [list set ${path}.plot.show.showScalesVar\
			    [::loon::listfns::booleanAs01\
				 [$plotWidget cget -showScales]]]
	}

	if {"showGuides" in $args} {
	    uplevel #0 [list set ${path}.plot.show.showGuidesVar\
			    [::loon::listfns::booleanAs01\
				 [$plotWidget cget -showGuides]]]
	}
	
	
	if {"showItemLabels" in $args} {
	    uplevel #0 [list set ${path}.plot.show.showItemLabelsVar\
			    [::loon::listfns::booleanAs01\
				 [$plotWidget cget -showItemLabels]]]
	}

	if {"selectionLogic" in $args} {
	    uplevel #0 [list set ${path}.select.controls.dynamicVar\
			    [$plotWidget cget -selectionLogic]]
	}

	if {"selectBy" in $args} {
	    uplevel #0 [list set ${path}.select.controls.byVar\
			    [$plotWidget cget -selectBy]]
	}

	

	if {"color" in $args || "active" in $args} {
	    
	    set cols [list]
	    
	    foreach col [$plotWidget cget -color]\
		a [$plotWidget cget -active] {
		    if {$a} {
			if {$col ni $cols} {
			    lappend cols $col
			}
		    }
		}
	    $selectByColorMenu configure -color $cols
	}       	
    }
    
}
