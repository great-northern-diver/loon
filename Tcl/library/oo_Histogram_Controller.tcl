
::oo::class create ::loon::classes::Histogram_Controller {
    
    superclass	::loon::classes::SweepBrush_Controller\
	::loon::classes::ZoomPan_Controller
    
    
    variable mode canvas
    
    method init {} {
	my variable view
	
	set mode "select"
	set canvas [$view getCanvas]

	next 
	
	set canvas [$view getCanvas]
    }
    
    method button1_press {x y} {
	
	set tags [$canvas gettags current]
	
	switch -- [lindex $tags 2] {
	    handle_origin {
		set mode origin
	    } 
	    handle_binwidth {
		set mode binwidth
	    }
	    default {
		set mode "select"
		next $x $y
	    }
	}	
	
    }

    method button1_motion {x y} {
	
	my variable map model view
	
	switch -- $mode {
	    origin {
		set new_origin [lindex [$map mapS2D $x $y] 0]
		uplevel #0 [list $model configure -origin $new_origin]
		if {[$model cget -yshows] eq "density"} {
		    $model scaleto world
		}
		update idletasks
	    }
	    binwidth {
		set bw [lindex [$map mapS2D $x $y] 0]
		set o [$model cget -origin]
		if {$bw > $o} {
		    uplevel #0 [list $model configure -binwidth [expr {$bw-$o}]]
		    if {[$model cget -yshows] eq "density"} {
			$model scaleto world
		    }
		    update idletasks
		}
	    }
	    default {
		next $x $y
	    }
	}
    }
    
    method dragBinwidth {x y} {
	puts "drag binwidth"
    }
    
}
