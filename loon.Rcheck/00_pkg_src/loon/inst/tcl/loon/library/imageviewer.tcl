
namespace eval loon {

    namespace export imageviewer
    
    proc imageviewer {images} {
	
	if {[llength $images] eq 0} {
	    error "length zero input"
	}
	
	## Do images exist
	set existingImages [image names]
	foreach image $images {
	    if {$image ni $existingImages} {
		error "Image \"$image\" does not exist."
	    }
	}
	
	set n [llength $images]
	
	## Create Viewer
	set instance 0
	while {[winfo exists .loon_imageview_$instance] eq 1} {
	    incr instance
	}
	
	set tt [toplevel .loon_imageview_$instance]
	wm title $tt "Image Viewer"
	
	## Save Images
	set ::loon::imageviewer::State($tt\.images) $images
	set ::loon::imageviewer::State($tt\.currentNr) 0
	
	## Create active image
	set ::loon::imageviewer::State($tt\.activeImage) [image create photo]
	
	## Draw UI
	
	set imageInfo [label $tt\.label -anchor w]
	set canvas [canvas $tt\.canvas -width 200 -height 300]
	set slider [scale $tt\.slider -from 0 -to [expr {$n-1}]\
			-orient "horizontal" -resolution 1]
	
	pack $imageInfo -side top -fill x -padx 2 -pady 2
	pack $slider -side bottom -fill x
	pack $canvas -side top -fill both -expand 1

	
	## Draw first Image
	::loon::imageviewer::displayImage $tt 0
	
	## Bind Slider Event
	$slider configure -command "::loon::imageviewer::displayImage $tt"
	
	## Bind Resize Event
	bind $canvas <Configure> "::loon::imageviewer::displayImage $tt -1"
	
	## Bind Keypress
	bind $tt <Key-Right> {
	    set viewer %W
	    set max [llength $::loon::imageviewer::State($viewer\.images)]
	    set nr $::loon::imageviewer::State($viewer\.currentNr)
	    if {$nr < [expr {$max -1}]} {
		incr nr
		$viewer\.slider set $nr
		::loon::imageviewer::displayImage $viewer $nr
	    }
	}
	
	bind $tt <Key-Left> {
	    set viewer %W
	    set nr $::loon::imageviewer::State($viewer\.currentNr)
	    if {$nr > 0} {
		incr nr -1
		$viewer\.slider set $nr
		::loon::imageviewer::displayImage $viewer $nr
	    }
	}
	
	return ""
    }
    
}

namespace eval loon::imageviewer {
   
    variable State

    proc displayImage {viewer nr} {
	variable State

	set mar 40
	
	if {$nr eq -1} {
	    set nr $State($viewer\.currentNr)
	}

	set image [lindex $State($viewer\.images) $nr]

	if {$image ni [image names]} {
	    error "Image \"$image\" does not exist."
	}
	
	set State($viewer\.currentNr) $nr
	
	set canvas $viewer\.canvas
	
	# Image and Canvas dimensions
	set width [image width $image]
	set height [image height $image]

	set cwidth [winfo width $canvas]
	set cheight [winfo height $canvas]

	set imageInfo $viewer\.label
	$imageInfo configure -text "$image: $width x $height"
	
	# Delete old Image
	$canvas delete all
	
	# Get New image dimensions
	set imgRatio [expr {double($width)/$height}]
	set canvasRatio [expr {double($cwidth)/$cheight}]
	
	if {$imgRatio<$canvasRatio} {
	    set image_h [expr {$cheight}]
	    set image_w [expr {double($image_h)*$imgRatio}]
	} else {
	    set image_w [expr {$cwidth-$mar}]
	    set image_h [expr {double($image_w)/$imgRatio}]
	}

	set activeImage $State($viewer\.activeImage)
	# scale image
	$::loon::Options(image_scale) $image [expr {int($image_w)}] [expr {int($image_h)}] $activeImage
		
	# display image on canvas
	$canvas create image\
	    [expr {$cwidth/2.0}] [expr {$cheight/2.0}]\
	    -image $activeImage -anchor c
	
    }
    
}
