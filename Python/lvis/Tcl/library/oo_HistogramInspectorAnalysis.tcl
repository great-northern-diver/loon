oo::class create loon::classes::HistogramInspectorAnalysis {

    superclass ::loon::classes::Inspector2
    
    variable colorMenuSelect colorMenuModify linkingGroupWidget\
	opt yshowsVar dynamicVar selectByVar

    
    constructor {Path} {
	
	next $Path
	
	my New_state modifycolor color any\
	    [::loon::listfns::toHexcolor $::loon::Options(colors)]

	my SetStateDescription modifycolor\
	    "color boxes shown in the modify color inspector part"
	
#	my variable modifycolor
#	$colorMenuModify configure -color $modifycolor	
    }

    
    method CheckNewActivewidget {widget} {
	if {![info object isa typeof $widget\
		  "::loon::classes::Histogram_Widget"]} {
	    error "$widget is not a histogram widget."
	}
    }

    method RegisterActivewidget {} {
	my variable activewidget
	
	next
	## update inspector
	if {$activewidget ne ""} {
	    my ActivewidgetEvents color
	}
	$linkingGroupWidget configure -activewidget $activewidget
    }

    
    method ActivewidgetEvents {args} {
	## args is events
	my variable activewidget

	if {$args eq "selected"} {
	    return
	}
	
	## TODO: fix, sometimes the events come nested
	if {[llength $args] eq 1} {
	    set args {*}$args
	}
	
	foreach state {showGuides swapAxes showLabels showScales showStackedColors showBinHandle showOutlines} {
	    set var ${opt}.${state}Var
	    set ::$var [::loon::listfns::booleanAs01 [uplevel #0 [list $activewidget cget -$state]]]	 
	}
	
	set ::$yshowsVar [uplevel #0 [list $activewidget cget -yshows]]
	set ::$dynamicVar [uplevel #0 [list $activewidget cget -selectionLogic]]
	set ::$selectByVar [uplevel #0 [list $activewidget cget -selectBy]]
	

	if {"color" in $args} {
	    set cols [uplevel #0 [list $activewidget cget -color]]
	    $colorMenuSelect configure -color\
		[lsort -unique $cols]
	}
	
    }
    
    
    method Make {} {
	my variable path

	frame $path -class LoonStarsInspectorAnalysis
	
	set pplot ${path}.plot
	set pselect ${path}.select
	set pmodify ${path}.modify
	pack [labelframe $pplot -text "Plot"]\
	    [labelframe $pselect -text "Select"]\
	    [labelframe $pmodify -text "Modify"]\
	    -side top -fill x -padx 5 -pady 2.5 -anchor w
	
	
	## Plot Options
	set opt [frame ${path}.plot.show]
	pack $opt -side top -fill x -expand TRUE
	
	label ${opt}.axesLabel -text "axes:"
	
	checkbutton ${opt}.swapAxesCB -text "swap"\
	    -variable ${opt}.swapAxesVar\
	    -onvalue 1 -offvalue 0\
	    -command "[self namespace]::my PlotCommands swapAxes ${opt}.swapAxesVar"

	checkbutton ${opt}.showScalesCB -text "scales"\
	    -variable ${opt}.showScalesVar\
	    -onvalue 1 -offvalue 0\
	    -command "[self namespace]::my PlotCommands showScales ${opt}.showScalesVar"

	checkbutton ${opt}.showGuidesCB -text "guides"\
	    -variable ${opt}.showGuidesVar\
	    -onvalue 1 -offvalue 0\
	    -command "[self namespace]::my PlotCommands showGuides ${opt}.showGuidesVar"
	
	
	checkbutton ${opt}.showLabelsCB -text "labels"\
	    -variable ${opt}.showLabelsVar\
	    -onvalue 1 -offvalue 0\
	    -command "[self namespace]::my PlotCommands showLabels ${opt}.showLabelsVar"
	
	label ${opt}.showLabel -text "show:"

	checkbutton ${opt}.showStackedColorsCB -text "stacked colors"\
	    -variable ${opt}.showStackedColorsVar\
	    -onvalue 1 -offvalue 0\
	    -command "[self namespace]::my PlotCommands showStackedColors ${opt}.showStackedColorsVar"

	checkbutton ${opt}.showBinHandleCB -text "bin handle"\
	    -variable ${opt}.showBinHandleVar\
	    -onvalue 1 -offvalue 0\
	    -command "[self namespace]::my PlotCommands showBinHandle ${opt}.showBinHandleVar"
	
	checkbutton ${opt}.showOutlinesCB -text "outlines"\
	    -variable ${opt}.showOutlinesVar\
	    -onvalue 1 -offvalue 0\
	    -command "[self namespace]::my PlotCommands showOutlines ${opt}.showOutlinesVar"

	
	label ${opt}.yshowsLabel -text "yshows:"
	
	set yshowsVar ${opt}.yshowsVar
	uplevel #0 [list set $yshowsVar frequency]
	set yshowsCmd "[self namespace]::my PlotCommands yshows $yshowsVar\n [self namespace]::my ScaleTo plot"
	frame ${opt}.yshows
	pack [radiobutton ${opt}.yshows.frequency -text "frequency"\
		  -variable $yshowsVar\
		  -value frequency -command $yshowsCmd]\
	    [radiobutton ${opt}.yshows.density -text "density"\
		 -variable $yshowsVar\
		 -value density -command $yshowsCmd]\
	    -side left

	## scaleto 
	label ${opt}.scaletoLabel -text "scale to:"
	frame ${opt}.scaleto
	
	#pack [button ${opt}.scaleto.selected -text "selected"\
	    #	  -command [list [self namespace]::my ScaleTo selected]]

	pack [button ${opt}.scaleto.points -text "plot"\
		  -command [list [self namespace]::my ScaleTo plot]]\
	    [button ${opt}.scaleto.world -text "world"\
		 -command [list [self namespace]::my ScaleTo world]]\
	    -side left
	


	## grid
	grid ${opt}.axesLabel     -row 0 -column 0 -sticky w
	grid ${opt}.swapAxesCB    -row 0 -column 1 -sticky w
	grid ${opt}.showScalesCB  -row 0 -column 2 -sticky w
	grid ${opt}.showGuidesCB  -row 1 -column 1 -sticky w
	grid ${opt}.showLabelsCB  -row 1 -column 2 -sticky w

	grid ${opt}.showLabel     -row 2 -column 0 -sticky w
	grid ${opt}.showStackedColorsCB   -row 2 -column 1 -sticky w
	grid ${opt}.showBinHandleCB   -row 2 -column 2 -sticky w	
	grid ${opt}.showOutlinesCB  -row 3 -column 1 -sticky w
	grid ${opt}.yshowsLabel   -row 4 -column 0 -sticky w
	grid ${opt}.yshows        -row 4 -column 1 -sticky w -columnspan 2 -pady 5

	grid ${opt}.scaletoLabel  -row 5 -column 0 -sticky w
	grid ${opt}.scaleto       -row 5 -column 1 -sticky w -columnspan 2
	
	set plinking [frame ${path}.plot.linking]
	set linkingGroupWidget ${plinking}.lgwidget
	pack $plinking -side top -anchor w -fill x -pady 5
	pack [label ${plinking}.label -text "linking group:"] -side left
	pack [::loon::linkingGroupWidget $linkingGroupWidget] -side left -padx 5 -fill x

	#pack [label ${path}.plot.colorStackOrderLabel -text "color stack order:"]\
	 #   -side top -anchor w

	
	# Select
	set sel [frame ${path}.select.controls]
	pack $sel -side top -fill x
	
	label ${sel}.staticLabel -text "static:"
	frame ${sel}.static
	pack [button ${sel}.static.all -text "all"\
		  -command [list [self namespace]::my SelectStatic all]]\
	    [button ${sel}.static.none -text "none"\
		 -command [list [self namespace]::my SelectStatic none]]\
	    [button ${sel}.static.invert -text "invert"\
		 -command [list [self namespace]::my SelectStatic invert]]\
	    -side left -anchor w
	
	label  ${sel}.dynamicLabel -text "dynamic:"
	frame ${sel}.dynamic

	set dynamicVar ${sel}.dynamicVar
	set cmdList [list [self namespace]::my SelectDynamic $dynamicVar]
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

	set selectByVar ${sel}.byVar
	set cmdList [list [self namespace]::my SelectBy ${sel}.byVar]
	pack [radiobutton ${sel}.by.sweeping -text "sweeping"\
		  -variable $selectByVar\
		  -value sweeping -command $cmdList]\
	    [radiobutton ${sel}.by.brushing -text "brushing"\
		 -variable $selectByVar\
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
	
	set colorMenuSelect\
	    [::loon::colormenu ${path}.select.byColor -hasAddColor 0]
	
	pack $colorMenuSelect -fill x -side top
	
	${colorMenuSelect}.canvas bind "color" <Button-1>\
	    [list [self namespace]::my SelectByColor FALSE]
	${colorMenuSelect}.canvas bind "color" <Shift-Button-1>\
	    [list [self namespace]::my SelectByColor TRUE]
	
	# Modify
	set mod ${path}.modify

	set colorMenuModify [::loon::colormenu ${path}.modify.color -command\
				 "[self namespace]::my ModifyColor %c"]
	pack $colorMenuModify -fill x -side top
	
	pack [frame ${mod}.active] -side top -anchor w
	
	pack [button ${mod}.active.deactivate -text "deactivate"\
		  -command [list [self namespace]::my ChangeActive deactivate]]\
	    [button ${mod}.active.reactivate -text "reactivate"\
		 -command [list [self namespace]::my ChangeActive reactivate]]\
	    -side left
	
    }

    method PlotCommands {state variable} {
	my variable activewidget
	if {$activewidget ne ""} {
	    uplevel #0 [list $activewidget configure -$state [set ::$variable]]
	}
    }
    
    method SelectStatic {which} {
	my variable activewidget
	if {$activewidget ne ""} {

	    switch -- $which {
		all {
		    uplevel #0 [list $activewidget configure -selected TRUE -which all]
		}
		none {
		    uplevel #0 [list $activewidget configure -selected FALSE -which all]
		}
		invert {
		    set selected\
			[::loon::listfns::booleanNot [::$activewidget cget -selected]]
		    uplevel #0 [list $activewidget configure -selected $selected]
		}
		default {
		    error "invalid which $which"
		}		
	    }
	}	
    }

    method SelectDynamic {var} {
	my variable activewidget
	if {$activewidget ne ""} {
	    uplevel #0 [list $activewidget configure -selectionLogic [set ::${var}]]
	}
    }

    method SelectBy {var} {
	my variable activewidget
	if {$activewidget ne ""} {
	    uplevel #0 [list $activewidget configure -selectBy [set ::${var}]]
	}
    }
    
    method SelectByColor {add} {
	my variable activewidget
	if {$activewidget eq ""} {
	    return
	}
	set canvas ${colorMenuSelect}.canvas
	set i [lindex [$canvas gettags current] 2]
	set sel_color [$canvas itemcget "color && inner && $i" -fill]
	
	set color [uplevel #0 [list $activewidget cget -color]]
	
	if {$add} {
	    set selected [uplevel #0 [list $activewidget cget -selected]]
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

	uplevel #0 [list $activewidget configure -selected $selected]
    }
    
    method ChangeActive {what} {
	my variable activewidget
	if {$activewidget ne ""} {
	    if {$what eq "deactivate"} {
		uplevel #0 [list $activewidget configure -active FALSE -which selected]
	    } else {
		uplevel #0 [list $activewidget configure -active TRUE]
	    }
	}
    }

    method ModifyColor {color} {
	my variable activewidget
	if {$activewidget ne ""} {
	    uplevel #0 [list $activewidget configure -color $color -which "selected"]
	}
    }
    

    method HookAfterStatesSet {} {
	my variable changedStates modifycolor
	if {"modifycolor" in $changedStates} {
	    $colorMenuModify configure -color $modifycolor
	}
	next
    }

    method ScaleTo {what} {
	my variable activewidget
	if {$activewidget ne ""} {
	    $activewidget scaleto $what
	}	
    }


}
