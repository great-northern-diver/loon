
oo::class create loon::classes::ImageGlyphVisual {
    
    superclass ::loon::classes::GlyphVisual
        
    variable images_var scaledImages\
	scaledImageHaloHalfWidths scaledImageHaloHalfHeights\
	glyphBinding 

    constructor {GlyphObject args} {

	## array with names being the area
	set scaledImages [dict create]
	array set scaledImageHaloHalfWidths {} 
	array set scaledImageHaloHalfHeights {}

	next $GlyphObject {*}$args
	
	set ns [info object namespace $GlyphObject]
	
	set images_var [${ns}::my varname images]
	
	
	set glyphBinding [${ns}::my systembind state add all\
			      "[self namespace]::my glyphChanged"]

	## scale images for some sizes
	puts "scale images..."
	foreach image [lsort -unique [set $images_var]] {
	    my AddNewImage $image	
	}
	puts "  done"
	
    }

    destroy {
	my variable glyphsObject

	dict for {key image} $scaledImages {
	    uplevel #0 [list image delete $image] 
	}
	if {$glyphBinding ne ""} {
	    uplevel #0 [list $glyphsObject systembind state delete $glyphBinding]
	}
    }

    
    ## this method needs to be called before the Scatterplot_View
    ## runs redraw model
    method glyphChanged {} {
	#puts "ImageGlyphVisual glyph has changed"
	
	set new [lsort -unique [set $images_var]]
	set old {}
	foreach img [dict keys $scaledImages] {
	    lappend old [regsub -- {-.*$} $img ""]
	}
	set old [lsort -unique $old]
	
	set toDelete [::loon::listfns::setDiff $new $old]
	set toAdd [::loon::listfns::setDiff $old $new]
	
	foreach img $toDelete {
	    my RemoveImage $img
	}
	
	foreach img $toAdd {
	    my AddNewImage $img
	}	
    }

    method AddNewImage {image} {
	
	set width [image width $image]
	set height [image height $image]
	

	set sizes [lsort -unique -real -decreasing $::loon::Options(image_sizes)]

	foreach area [::loon::map_image_size $sizes] {
	    set scale [expr {sqrt(double($area)/($width*$height))}]
	    set image_w [expr {int($scale*$width)}]
	    set image_h [expr {int($scale*$height)}]
	    
	    set scaled_img [image create photo]
	    $::loon::Options(image_scale) $image $image_w $image_h $scaled_img
	    
	    set scaledImageHaloHalfWidths($scaled_img) [expr {$image_w/2.0+2}]
	    set scaledImageHaloHalfHeights($scaled_img) [expr {$image_h/2.0+2}]
	    
	    dict set scaledImages ${image}-${area} $scaled_img
	    
	    if {$scale < 1} {
		set image $scaled_img
		set width $image_w
		set height $image_h
	    }
	}
    }

    method RemoveImage {image} {
	
	set imgkeys [dict key $scaledImages ${image}-*]
	foreach k $imgkeys {
	    set img [dict get $scaledImages $k]
	    
	    uplevel #0 [list image delete $img]
	    array unset scaledImageHaloHalfWidth $img
	    
	    dict unset scaledImages $k
	}
	
    }

    method AddImageArea {image area} {

	## use next larger image size  to scale down if available
	set availableImages [dict keys $scaledImages ${image}-*]
	set sl [string length $image]
	incr sl ;# because of -
	set availableAreas {}
	foreach img $availableImages {
	    lappend availableAreas [string range $img $sl end]
	}
	set availableAreas [lsort -real -decreasing $availableAreas]
	
	## find next larger image, otherwise use original image
	set useArea ""
	foreach a $availableAreas {
	    if {$area > $a} {
		set useArea $a
	    } else {
		break
	    }
	}
	
	if {$useArea eq ""} {
	    set useImage $image
	} else {
	    set useImage [dict get $scaledImages ${image}-$useArea]
	}
	
	set width [image width $useImage]
	set height [image height $useImage]
	
	set scale [expr {sqrt(double($area)/($width*$height))}]
	set image_w [expr {int($scale*$width)}]
	set image_h [expr {int($scale*$height)}]
	
	set scaled_img [image create photo]
	$::loon::Options(image_scale) $image $image_w $image_h $scaled_img
	
	set scaledImageHaloHalfWidths($scaled_img) [expr {$image_w/2.0+2}]
	set scaledImageHaloHalfHeights($scaled_img) [expr {$image_h/2.0+2}]

	dict set scaledImages ${image}-$area $scaled_img
    }
    
    method draw {canvas ind} {
	
	
	set id {}
	
	lappend id [$canvas create rect 0 0 0 0\
			-width 6]
				
	lappend id [$canvas create image 0 0 -anchor c]
	
	return $id
    }
    
    method updateCoords {id canvas ind size} {
	
	set imageArea [::loon::map_image_size $size]
	set image [lindex [set $images_var] $ind]
	set imgKey ${image}-$imageArea
	
	if {![dict exists $scaledImages $imgKey]} {
	    my AddImageArea $image $imageArea
	}
	
	set screenImg [dict get $scaledImages $imgKey]
	
	$canvas coords [lindex $id 0]\
	    -$scaledImageHaloHalfWidths($screenImg)\
	    -$scaledImageHaloHalfHeights($screenImg)\
	    $scaledImageHaloHalfWidths($screenImg)\
	    $scaledImageHaloHalfHeights($screenImg)
	
	$canvas coords [lindex $id 1] 0 0
	$canvas itemconfigure [lindex $id 1] -image $screenImg
    }
    
    
    method recolor {id canvas ind color} {
	$canvas itemconfigure [lindex $id 0]\
	    -fill $color -outline $color
    }
    
    method InfoDebug {args} {
	next images_var scaledImages {*}$args
    }
    
}
