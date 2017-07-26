oo::class create loon::classes::PointrangeGlyphInspector {

    superclass ::loon::classes::GlyphInspector
    
    method Make {} {
	my variable path
	
	frame $path -class PointrangeGlyphInspector
	
	foreach state {showArea} {
	    my variable cb_${state}
	    set var [my varname cb_$state]; # need absolute path
	    checkbutton ${path}.$state -text $state\
		-variable $var -onvalue TRUE -offvalue FALSE\
		-command "[self namespace]::my PlotCommands $state"
	    pack ${path}.$state -side top -anchor w 
	} 
    }

    method ActiveglyphEvents {events} {
	my variable activewidget activeglyph
	
	## for now always update all
	foreach state {showArea} {
	    my variable cb_$state
	    set cb_$state [uplevel #0 [list $activewidget glyph use $activeglyph cget -$state]]
	}
    }
    
    method PlotCommands {state} {
	my variable activewidget cb_$state activeglyph
	if {$activewidget ne "" && $activeglyph ne ""} {
	    uplevel #0 [list $activewidget glyph use $activeglyph\
			    configure -$state [set cb_${state}]]
	}
    }


    
    
}
