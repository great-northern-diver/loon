oo::class create loon::classes::NavigatorInspector {

    superclass ::loon::classes::Inspector2

    variable glyphs
    
    constructor {Path} {
	
	next $Path
	
    }
    
    method CheckNewActivewidget {widget} {
	if {![info object isa typeof $widget\
		  "::loon::classes::withGlyphs"]} {
	    error "$widget does not support glyphs."
	}
    }
    

    method Make {} {
	my variable path
	
	frame $path -class SerialaxesGlyphInspector
	
	pack [::tk::labelframe ${path}.lframe -text "Serialaxes"] -fill x

	set ppath ${path}.lframe
	
	pack [label ${ppath}.l2 -text "showAxes"]\
	    [label ${ppath}.l3 -text "axesColor"]\
	    [label ${ppath}.l5 -text "linewidth"]\
	    [label ${ppath}.l6 -text "scaling"]\
	    [label ${ppath}.l7 -text "axes layout"]\
	    [label ${ppath}.l8 -text "showArea"]\
	    -side top -anchor w
    } 
    
}
