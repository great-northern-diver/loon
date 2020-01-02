
::oo::class create ::loon::classes::Graph_View {
    
    superclass ::loon::classes::withCanvasAndItemBindings\
	::loon::classes::Decorated_View
	
    
    variable controller
    
    constructor {Path} {
	
	
	next $Path
	
	set controller [::loon::classes::Graph_Controller new [self]]
	
    }

    method setPlotModel {Model} {
	
	next $Model	

	$controller setModel $Model
    }
    

   
}
