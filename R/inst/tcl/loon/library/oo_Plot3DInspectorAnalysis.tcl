
oo::class create loon::classes::Plot3DInspectorAnalysis {
    
    superclass ::loon::classes::PlotInspectorAnalysis
    
    constructor {path} {
        next $path
        
    }
    
    method Make {} {
        my variable path
        
        next
        
        set modf ${path}.modify.frame
        set imgResetRotation [image create bitmap\
                  -file [file join $::env(LOON_LIBRARY)\
                         images resetRotation.xbm]]
        
        # Add rotation reset button to normal analysis inspector
        pack [button ${modf}.move.resetRotation\
             -image $imgResetRotation\
             -command [list [self namespace]::my movePoints resetRotation]]\
            -side left
	
	}    
}
