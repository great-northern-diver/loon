oo::class create loon::classes::PlotInspectorNodesAnalysis {
    
    superclass ::loon::classes::Inspector2
    
    variable colorChooser

    constructor {path} {
	set plotWidget ""
	
	next $path
	
    }

    
}
