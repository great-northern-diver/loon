
oo::class create loon::classes::Bindings {

    variable idPrefix nextIdNr Events Code Order\
	substitutions

    superclass loon::classes::fancyInfo

    constructor {IDPrefix} {
	set idPrefix $IDPrefix
	set nextIdNr 0
	set Order {}
	array set Events {}
	array set Code {}
	set substitutions {}
	
	next
    }
    
    # Right now, execute "code" for any of the events
    # (logical OR)
    method add {events code} {	
	set id [format "%s%s" $idPrefix $nextIdNr]
	incr nextIdNr
	
	set Events($id) $events
	set Code($id) $code
	lappend Order $id
	
	#puts [format "%s binding %s added" [self] $id]
	

	return $id
    }
    
    method delete {id} {
	if {$id eq "all"} {
	    array set Events {}
	    array set Code {}
	    set Order {}
	} else {
	    set i [lsearch -exact $Order $id]
	    if {$i ne -1} {
		array unset Events $id
		array unset Code $id
		set Order [lreplace $Order $i $i] 
	    } else {
		error "id: $id does not exist."
	    }
	}
	## puts [format "%s binding %s deleted" [self] $id]
    }

    method reorder {ids} {
	
	if {[llength $ids] ne [llength [lsort -unique $ids]]} {
	    error "binding ids are not unique."
	}
	
	set tooMuch [::loon::listfns::setDiff $ids $Order]
	if {[llength $tooMuch] ne 0} {
	    error "the bindings \"$tooMuch\" do not exist."
	}
	
	set tooLittle [::loon::listfns::setDiff $Order $ids]
	if {[llength $tooLittle] ne 0} {
	    error "the bindings \"$tooLittle\" are missing."
	}
	
	set Order $ids
    }
    
    method get {args} {
	switch -- [llength $args] {
	    0 {
		return $Order
	    }
	    1 {
		set id $args
		if {$args in $Order} {
		    return [list $Events($id) $Code($id)]
		} else {
		    error "Binding $id does not exist."
		}
	    }
	    default {
		error "binding"
	    }
	    
	}
	
    }
    
    
    method setSubstitutions {subst} {
	foreach {k v} $subst {
	    if {[string range $k 0 0] ne "%"} {
		error "substitution keys must start with %"
	    }
	}
	set substitutions $subst
    }
    
    method addSubstitutions {subst} {
	my setSubstitutions [list {*}$substitutions {*}$subst]	
    }

    method getSubstitutions {subst} {
	return $substitutions
    }
 
    method InfoDebug {args} {
	next idPrefix Order Events substitutions {*}$args
    }
    

}
