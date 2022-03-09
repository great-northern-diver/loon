
::oo::class create ::loon::classes::TextWorldviewVisual { 
    
    superclass ::loon::classes::TextVisual


    method updateItem {} {
	my variable isVisible canvas id\
	    text_var color_var angle_var tag_var visualid
	
	set tag [list layer $visualid {*}[set $tag_var]]
	
	## No size consideration	
	if {$isVisible} {	    
	    uplevel #0 [list $canvas itemconfigure $id\
			    -tag $tag\
			    -text [set $text_var]\
			    -fill {*}[set $color_var]\
			    -angle [set $angle_var]\
			    -font "$::loon::Options(font) 8"]
	}
    }	
}
