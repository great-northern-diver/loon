namespace eval loon {
    namespace export resize 
    proc resize {widget width height {window TRUE}} {
	if {$window} {
	    wm geometry [winfo toplevel $widget] "${width}x${height}"
	} else {
	    ${widget}.canvas configure -with $width $height
	}
    }
}
