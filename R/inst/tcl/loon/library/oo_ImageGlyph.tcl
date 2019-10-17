
oo::class create loon::classes::ImageGlyph {
    
    superclass ::loon::classes::Glyph

    constructor {} {
	next
	my New_state images image n ""

	my SetStateDescription images\
	    "tcl image ids, i.e. in \[image names\]"
    }
    

    ## the image type is not defined in Configurable
    method Check_image {var values args} {

	## check if image exists
	set existingImages [image names]
	foreach img $values {
	    if {$img ni $existingImages} {
		error "-image \"$img\" does not exist."
	    }    
	}
	
	## TODO make better
	my variable stateDimInfo
	set fixed_n [dict get $stateDimInfo n fixedDim]

	set nval [llength $values]
	if {$nval eq 1} {
	    set values [lrepeat $fixed_n $values]
	} elseif {$nval ne $fixed_n} {
	    error "-$var has length $nval but length $fixed_n or 1 is expected."   
	}

	return $values
    }
      
}
