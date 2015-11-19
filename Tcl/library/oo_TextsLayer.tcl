
::oo::class create ::loon::classes::TextsLayer {
    
    superclass ::loon::classes::PrimitiveLayerN

    constructor {Container} {
	my variable type
	
	next $Container FALSE

	set type "texts"

	my New_state color color n "black"
	my New_state text string n ""
	my New_state angle double n 0
	my New_state size positive_integer n 8
	my New_state anchor factor n center {n ne e se s sw w nw center}
	my New_state justify factor n center {left center right}


	my SetStateDescription text\
	    "text string (note that for n=1 no spaces are allowed)"
	
	my SetStateDescription color\
	    "text color"
	
	my SetStateDescription angle\
	    "text rotation angle"
	
	my SetStateDescription anchor\
	    "specifies how multiline text is anchored. Choice of: left center right"
	
	my SetStateDescription justify\
	    "specifies how to justify the text. Choice of: n ne e se s sw w nw center"


    }

    method GetDefaultValue {state length} {	
	switch -- $state {
	    text {
		return [lrepeat $length {}]
	    }
	    default {
		return [next $state $length]
	    }
	}
    }

    
}
