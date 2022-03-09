
oo::class create loon::classes::SerialaxesGlyphInspector {

    superclass ::loon::classes::GlyphInspector

    variable axesLayout scaling


    method Make {} {
	my variable path

	frame $path -class SerialaxesGlyphInspector

	foreach state {showEnclosing showAxes showArea andrews} {
	    my variable cb_${state}
	    set var [my varname cb_$state]; # need absolute path
	    checkbutton ${path}.$state -text $state\
		-variable $var -onvalue TRUE -offvalue FALSE\
		-command "[self namespace]::my PlotCommands $state"
	    pack ${path}.$state -side top -anchor w
	}

	## axes layout
	set axesLayout "radial"
	set lvar [my varname axesLayout]
	set playout ${path}.axesLayout
	pack [frame $playout] -side top -anchor w
	pack [label ${playout}.label -text "axes layout:"]\
	    [radiobutton ${playout}.radial -text "radial"\
		 -variable $lvar -value "radial"\
		 -command "[self namespace]::my ChangeAxesLayout"]\
	    [radiobutton ${playout}.parallel -text "parallel"\
		 -variable $lvar -value "parallel"\
		 -command "[self namespace]::my ChangeAxesLayout"]\
	    -side left

	## scaling
	pack [label ${path}.l_scaling -text "scaling:"] -side top -anchor w
	set pscl [frame ${path}.f_scaling]
	pack $pscl -side top -anchor w

	set scl [list variable observation data none]
	set wscl {}
	set vscl [my varname scaling]
	set scaling "variable"
	foreach s $scl {
	    lappend wscl [radiobutton ${pscl}.${s} -text $s\
			      -variable $vscl -value $s\
			      -command "[self namespace]::my ChangeScaling"]
	}
	pack {*}$wscl -side top -anchor w


	# pack  [label ${ppath}.l2 -text "showAxes"]\
	#     [label ${ppath}.l3 -text "axesColor"]\
	#     [label ${ppath}.l5 -text "linewidth"]\
	#     [label ${ppath}.l6 -text "scaling"]\
	#     [label ${ppath}.l7 -text "axes layout"]\
	#     [label ${ppath}.l8 -text "showArea"]\
	    #     -side top -anchor w
    }

    method ActiveglyphEvents {events} {
	my variable activewidget activeglyph

	## for now always update all
	foreach state {showEnclosing showAxes showArea andrews} {
	    my variable cb_$state
	    set cb_$state [uplevel #0 [list $activewidget glyph use $activeglyph cget -$state]]
	}

	set axesLayout [uplevel #0 [list $activewidget glyph use $activeglyph cget -axesLayout]]
	set scaling [uplevel #0 [list $activewidget  glyph use $activeglyph cget -scaling]]
    }


    method PlotCommands {state} {
	my variable activewidget cb_$state activeglyph
	if {$activewidget ne "" && $activeglyph ne ""} {
	    uplevel #0 [list $activewidget glyph use $activeglyph\
			    configure -$state [set cb_${state}]]
	}
    }

    method ChangeAxesLayout {} {
	my variable activewidget activeglyph
	if {$activewidget ne "" && $activeglyph ne ""} {
	    uplevel #0 [list $activewidget glyph use $activeglyph configure -axesLayout $axesLayout]
	}

    }
    method ChangeScaling {} {
	my variable activewidget activeglyph
	if {$activewidget ne "" && $activeglyph ne ""} {
	    uplevel #0 [list $activewidget glyph use $activeglyph configure -scaling $scaling]
	}

    }
}
