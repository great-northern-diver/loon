
package require Tk

namespace eval loon {

    ## This Code is Explained in Effective Tcl/Tk Programming
    ## by Mark Harrison and Michael McLennan
    
    ::tk::toplevel .loon_ballonhelp -background black\
	-borderwidth 1 -relief flat
    
    label .loon_ballonhelp.info -justify left
    pack .loon_ballonhelp.info -side top -anchor nw -fill y
    wm overrideredirect .loon_ballonhelp 1
    wm withdraw .loon_ballonhelp


    variable loon_labels
    set loon_labels [dict create active 1]
    
       
    proc loonlabel_for_widget {widget message} {
	variable loon_labels
	
	dict set loon_labels $widget $message
	bind $widget <Enter> {+::loon::loonlabel_pending widget %W}
	bind $widget <Leave> {+::loon::loonlabel_cancel}
	
    }  
    
    
    proc loonlabel_pending {type widget {message ""}} {
	variable loon_labels

	if {[dict get $loon_labels active]} {

	    ::loon::loonlabel_cancel

	    switch -- $type {
		canvas {
		    set time 800
		}
		widget {
		    set time 1500
		}
		
	    }

	    dict set loon_labels pending\
		[after $time [list ::loon::loonlabel_show $type $widget $message]]

	}
	
    }

    proc loonlabel_cancel {} {
	variable loon_labels
	
	if {[dict exists $loon_labels pending]} {
	    after cancel [dict get $loon_labels pending]
	    dict unset loon_labels pending
	}
	
	wm withdraw .loon_ballonhelp
	update
    }

    proc loonlabel_show {type widget {message ""}} {
	variable loon_labels

	if {[dict get $loon_labels active]} {
	    switch -- $type {
		canvas {
		    set msg $message
		}
		widget {
		    set msg [dict get $loon_labels $widget]
		}
	    }
	    
	    if {$msg ne ""} {
		.loon_ballonhelp.info configure -text $msg
		
		set x [expr {[winfo pointerx $widget]+10}]
		set y [expr {[winfo pointery $widget]+10}]
		
		wm geometry .loon_ballonhelp +$x+$y
		wm deiconify .loon_ballonhelp
		raise .loon_ballonhelp
	    }
	}
	dict unset loon_dict pending	
    }

    # button .b1 -text "Hello"
    # pack .b1 -side left
    # button .b2 -text "World"
    # pack .b2 -side left
    # ::loon::loonlabel_for_widget .b1 "Hello Label 1"
    # ::loon::loonlabel_for_widget .b2 "This is a Test"

    # canvas .c -bg thistle
    # pack .c -side top
    # .c create rect 20 20 50 50 -fill blue -tag A
    # .c create oval 89 89 120 120 -fill orange -tag B
    # ::loon::loonlabel_for_itemtag .c A
    
}
