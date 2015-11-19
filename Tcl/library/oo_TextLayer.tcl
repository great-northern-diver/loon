
::oo::class create ::loon::classes::TextLayer {
    
    superclass ::loon::classes::PrimitiveLayer1
    ## careful x and y have dim 2

    constructor {Container} {
	my variable type
	
	next $Container

	set type "text"
	
	my New_state text string 1 ""
	my New_state color color n black

	my New_state angle double 1 0
	my New_state size positive_integer 1 8
	my New_state anchor factor 1 center {n ne e se s sw w nw center}
	my New_state justify factor 1 center {left center right}


	my SetStateDescription text\
	    "text string"
	
	my SetStateDescription color\
	    "text color"
	
	my SetStateDescription angle\
	    "text rotation angle"
	
	my SetStateDescription anchor\
	    "specifies how multiline text is anchored. Choice of: left center right"
	
	my SetStateDescription justify\
	    "specifies how to justify the text. Choice of: n ne e se s sw w nw center"

	
    }
    
}
