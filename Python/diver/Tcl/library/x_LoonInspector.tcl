
namespace eval loon {
    
    variable loonInspector
    set loonInspector ""

    proc setLoonInspectorActiveWidget {widget} {
	variable loonInspector
	variable open_widgets
		

	if {[$widget cget -useLoonInspector]} {
	    if {[llength $loonInspector] eq 0} {
		set loonInspector [::loon::loon_inspector -activewidget $widget]
		wm title [winfo toplevel $loonInspector] "Loon Inspector"
		update
	    }
	    
	    if {[$loonInspector cget -activewidget] ne $widget} {
		$loonInspector configure -activewidget $widget
	    }
	}
    }    
    
}
