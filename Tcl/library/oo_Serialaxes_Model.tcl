
oo::class create ::loon::classes::Serialaxes_Model {
    
    superclass ::loon::classes::Serialaxes\
	::loon::classes::withItemlabels\
	::loon::classes::Linkable
	
    
    constructor {args} {
	
	next {*}$args

	# method New_state {name type dim init args} ...
	my New_state active       boolean n TRUE
	my New_state color        color n steelblue
	my New_state selected     boolean n FALSE

	my New_state showGuides   boolean 1 TRUE
	
	my New_state showLabels     boolean 1 FALSE
		
	my New_state useLoonInspector boolean 1 TRUE
	
	my New_state title        string 1 ""
	my New_state tag          string n ""
	
	my New_state bbox double 4 {0 0 1 1}
	my New_state panX double 1 0
	my New_state panY double 1 0
	my New_state zoomX positive_double 1 1
	my New_state zoomY positive_double 1 1
	
	
	my setLinkedStates [list active color selected]
    }
    
      
    method GetDefaultValue {state length} {
	if {$state eq "tag"} {
	    if {$length eq 0} {
		return {}
	    } else {
		set tags {} 
		for {set i 0} {$i < $length} {incr i} {
		    lappend tags glyph$i
		}		
		return $tags
	    }
	} else {
	    next $state $length
	}
    }
    
    

}
