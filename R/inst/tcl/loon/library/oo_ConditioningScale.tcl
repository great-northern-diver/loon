
oo::class create loon::classes::ConditionalScale {

    superclass ::loon::classes::Inspector2
    
    variable canvas 
    
    constructor {Path} {

	set canvas ""

	next $Path

	my New_state orient factor 1 vertical {vertical horizontal}

	my New_state from double 1 0
	my New_state to double 1 1

	my New_state ngroups positive_integer 1 3

	my New_state splitAt double 1 {.3 .7}

    }

     method Make {} {
	my variable path
	
	frame $path -class LoonLadder
	set canvas [canvas ${path}.canvas -bg white]
	pack $canvas -fill y -expand TRUE
    }
    

}
