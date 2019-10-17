
::oo::class create ::loon::classes::TextsWorldviewVisual { 
    
    superclass ::loon::classes::TextsVisual


    method updateItems {} {
	my variable isVisible canvas ids n_var\
	    text_var color_var angle_var anchor_var justify_var

	## No size consideration
	if {$isVisible && [set $n_var] ne 0} {

	    foreach id $ids\
		color [set $color_var]\
		text [set $text_var]\
		angle [set $angle_var]\
		anchor [set $anchor_var]\
		justify [set $justify_var]\
		tag [my getTags] {
		    uplevel #0 [list $canvas itemconfigure $id\
				    -tag $tag\
				    -text $text\
				    -fill $color\
				    -angle $angle\
				    -anchor $anchor\
				    -justify $justify\
				    -font "$::loon::Options(font) 8"]
		}
	}
    }
    
    
}
