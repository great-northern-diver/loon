
::oo::class create ::loon::classes::PrimitiveLayer1 {
    
    superclass ::loon::classes::PrimitiveLayer


    constructor {Container} {

	next $Container

	my New_state x double n "" 
	my New_state y double n "" 
	my SetInitStates n {x y}

	my New_state tag string any ""

	my New_state itemLabel string any "item label"
	
	my SetStateDescription x\
	    "x coordinates"

	my SetStateDescription y\
	    "y coordinates"	

	my SetStateDescription tag\
	 "tags useful for item bindings"
    }
    
}
