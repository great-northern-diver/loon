
oo::class create loon::classes::SerialaxesInspectorAnalysis {

    superclass ::loon::classes::Inspector2

    variable imgPlus imgMinus scaling\
	colorMenuSelect colorMenuModify lwd axesLayout linkingGroupWidget

    constructor {Path} {

	set imgPlus [image create bitmap\
			 -file [file join $::env(LOON_LIBRARY)\
				    images plus.xbm]]
	set imgMinus [image create bitmap\
			  -file [file join $::env(LOON_LIBRARY)\
				     images minus.xbm]]

	set scaling observation
	set axesLayout radial

	next $Path

	my New_state modifycolor color any\
	    [::loon::listfns::toHexcolor $::loon::Options(colors)]

	my variable modifycolor
	$colorMenuModify configure -color $modifycolor

	my SetStateDescription modifycolor\
	    "color boxes shown in the modify color inspector part"

    }


    method CheckNewActivewidget {widget} {
	if {![info object isa typeof $widget\
		  "::loon::classes::Serialaxes_Widget"]} {
	    error "$widget is not a Serialaxes Widget."
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

	## TODO: fix, sometimes the events come nested
	if {[llength $args] eq 1} {
	    set args {*}$args
	}

	foreach state {showGuides showAxes showAxesLabels showLabels showItemLabels showArea andrews} {
	    my variable cb_$state
	    set cb_$state [uplevel #0 [list $activewidget cget -$state]]
	}

	set axesLayout [uplevel #0 [list $activewidget cget -axesLayout]]
	set scaling [uplevel #0 [list $activewidget cget -scaling]]

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


	foreach state {showGuides showAxes showAxesLabels showLabels showItemLabels showArea andrews} {
	    my variable cb_${state}
	    set var [my varname cb_$state]; # need absolute path
	    checkbutton ${pplot}.$state -text $state\
		-variable $var -onvalue TRUE -offvalue FALSE\
		-command "[self namespace]::my PlotCommands $state"
	    pack ${pplot}.$state -side top -anchor w
	}


	## axes layout
	set lvar [my varname axesLayout]
	set playout [frame ${path}.plot.f_layout]

	pack $playout -side top -anchor w
	pack [label ${playout}.label -text "axes layout:"]\
	    [radiobutton ${playout}.radial -text "radial"\
		 -variable $lvar -value "radial"\
		 -command "[self namespace]::my ChangeAxesLayout"]\
	    [radiobutton ${playout}.parallel -text "parallel"\
		 -variable $lvar -value "parallel"\
		 -command "[self namespace]::my ChangeAxesLayout"]\
	    -side left

	## Scaling
	pack [label ${path}.plot.l_scaling -text "scaling:"] -side top -anchor w
	set pscl [frame ${path}.plot.f_scaling]
	pack $pscl -side top -anchor w

	set scl [list variable observation data none]
	set wscl {}
	set vscl [my varname scaling]
	foreach s $scl {
	    lappend wscl [radiobutton ${pscl}.${s} -text $s\
			      -variable $vscl -value $s\
			      -command "[self namespace]::my ChangeScaling $s"]
	}
	pack {*}$wscl -side top -anchor w

	set plinking [frame ${path}.plot.linking]
	set linkingGroupWidget ${plinking}.lgwidget
	pack $plinking -side top -anchor w
	pack [label ${plinking}.label -text "linking group:"] -side left
	pack [::loon::linkingGroupWidget $linkingGroupWidget] -side left -padx 5


	## Selection
	set ssel ${path}.select.static
	pack [frame $ssel] -side top -anchor w

	set btns {}

	foreach b {all none invert} {
	    lappend btns [button ${ssel}.$b\
			      -text $b -command "[self namespace]::my SelectStatic $b"]
	}
	pack {*}$btns -side left

	pack [label ${path}.select.l_bycolor -text "by color:"] -side top -anchor nw
	set colorMenuSelect [::loon::colormenu ${path}.select.bycolor -color {}\
				 -hasAddColor FALSE]
	pack $colorMenuSelect -fill x -side top

	${colorMenuSelect}.canvas bind "color" <Button-1>\
	    [list [self namespace]::my SelectByColor FALSE]
	${colorMenuSelect}.canvas bind "color" <Shift-Button-1>\
	    [list [self namespace]::my SelectByColor TRUE]



	## Modify
	set mactive ${path}.modify.active
	pack [frame $mactive] -side top -anchor w
	pack [button ${mactive}.deactivate -text "deactivate"\
		  -command "[self namespace]::my ChangeActive deactivate"]\
	    [button ${mactive}.reactivate -text "reactivate"\
		 -command "[self namespace]::my ChangeActive reactivate"]\
	    -side left

	set colorMenuModify [::loon::colormenu ${path}.modify.color -command\
				 "[self namespace]::my ModifyColor %c"]
	pack $colorMenuModify -fill x -side top

	## Linewidth
	set mlwd ${path}.modify.f_lwd
	pack [frame $mlwd] -side top -anchor w -pady 5
	pack [label ${mlwd}.l_lwd -text "linewidth:"] -side left -padx {0 5}


	label  ${mlwd}.absLabel -text "abs:"
	button ${mlwd}.absm1 -image $imgMinus\
	    -command "[self namespace]::my ChangeLinewidth abs-1"
	button ${mlwd}.absp1 -image $imgPlus\
	    -command "[self namespace]::my ChangeLinewidth abs+1"

	label  ${mlwd}.relLabel -text "rel:"
	button ${mlwd}.relm1 -image $imgMinus\
	    -command "[self namespace]::my ChangeLinewidth rel-1"
	button ${mlwd}.relp1 -image $imgPlus\
	    -command "[self namespace]::my ChangeLinewidth rel+1"


	pack ${mlwd}.absLabel\
	    ${mlwd}.absm1\
	    ${mlwd}.absp1\
	    -side left

	pack ${mlwd}.relLabel\
	    ${mlwd}.relm1\
	    ${mlwd}.relp1\
	    -side left -padx {5 0}

    }

    method PlotCommands {state} {
	my variable activewidget cb_$state
	if {$activewidget ne ""} {
	    uplevel #0 [list $activewidget configure -$state [set cb_${state}]]
	}
    }

    method ChangeScaling {what} {
	my variable activewidget
	if {$activewidget ne ""} {
	    uplevel #0 [list $activewidget configure -scaling $scaling]
	}
    }

    method ChangeLinewidth {how} {
	my variable activewidget
	if {$activewidget ne ""} {
	    set lwd {}
	    set oldLwd [::loon::listfns::subsetLogical\
			     [$activewidget cget -linewidth]\
			     [$activewidget cget -selected]]

	    if {[llength $oldLwd] eq 0} {
		return
	    }

	    set n [llength $oldLwd]

	    switch -- $how {
		abs+1 {
		    set min [::loon::listfns::min $oldLwd]
		    set lwd [lrepeat $n [expr {$min + 1}]]
		}
		abs-1 {
		    set min [::loon::listfns::min $oldLwd]
		    if {$min <= 1} {
			bell
			return
		    } else {
			set lwd [lrepeat $n [expr {$min - 1}]]
		    }
		}
		rel+1 {
		    foreach e $oldLwd {
			lappend lwd [expr {$e+1}]
		    }
		}
		rel-1 {
		    set min [::loon::listfns::min $oldLwd]
		    if {$min <= 1} {
			bell
			return
		    } else {
			foreach e $oldLwd {
			    lappend lwd [expr {$e-1}]
			}
		    }
		}
	    }
	    uplevel #0 [list $activewidget configure -linewidth $lwd -which selected]
	}
    }


    method SelectStatic {what} {
	my variable activewidget
	if {$activewidget ne ""} {
	    switch -- $what {
		all {
		    uplevel #0 [list $activewidget configure -selected TRUE]
		}
		none {
		    uplevel #0 [list $activewidget configure -selected FALSE]
		}
		invert {
		    set sel [uplevel #0 [list $activewidget cget -selected]]
		    uplevel #0 [list $activewidget configure -selected\
				    [::loon::listfns::booleanNot $sel]]
		}
	    }

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

    method ChangeAxesLayout {} {
	my variable activewidget
	if {$activewidget ne ""} {
	    uplevel #0 [list $activewidget configure -axesLayout $axesLayout]
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


}
