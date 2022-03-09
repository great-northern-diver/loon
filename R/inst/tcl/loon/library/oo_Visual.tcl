
::oo::class create ::loon::classes::Visual { 
    
    superclass ::loon::classes::fancyInfo
    
    variable isVisible canvas map visualid
    
    
    constructor {Visualid Canvas Map {IsVisible TRUE}} {


	set visualid $Visualid
	set canvas $Canvas
	set map $Map
	set isVisible $IsVisible
	
		
	next 
	
	my redraw
	
    }

    destructor {
	## catch because canvas could already be destroyed
	catch {my clear}
    }
    
    method InfoDebug {args} {
	next isVisible canvas map visualid {*}$args
    }
    
    method clear {} {       
	if {$visualid ne ""} {
	    $canvas delete $visualid
	}	
    }
    
    ## Abstract
    method redraw {} {}
    method updateCoords {} {}
    method updateZoomPan {oldPanX oldPanY oldZoomX oldZoomY} {}
    
    ## Important: the the View moves the stuff this method may make some adjustments 
    method updatedPan {dPanX dPanY} {}
    
    
    method setVisibility {value} {
	set isVisible $value
	
	if {$isVisible} {
	    uplevel #0 [list $canvas itemconfigure $visualid -state normal]
	    my updateCoords
	} else {
	    uplevel #0 [list $canvas itemconfigure $visualid -state hidden]
	}
    }
}
