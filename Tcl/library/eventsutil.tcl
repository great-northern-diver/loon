

namespace eval loon {

    namespace export eventsutil
    
    proc eventsutil {} {
	set t [::loon::loon_toplevel]
	canvas ${t}.c -bg thistle
    
	pack ${t}.c -fill both -expand TRUE
	
	foreach meta {"" Command Shift Control Alt Option} {
	    foreach mouse {Button-1 Button-2 Button-3 Button-4 Button-5 MouseWheel\
			       Button1-Motion Button2-Motion Button3-Motion Button4-Motion Button5-Motion} {
		
		if {$meta eq ""} {
		    set eventpattern "<${mouse}>"
		} else {
		    set eventpattern "<${meta}-${mouse}>"
		}
		bind ${t}.c $eventpattern [list puts "Event: $eventpattern"]	
	    }    
	}
    }
    
}
