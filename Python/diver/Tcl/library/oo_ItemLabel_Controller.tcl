

::oo::class create ::loon::classes::ItemLabel_Controller {

    variable showItemLabels_var itemLabel_var

    superclass ::loon::classes::Canvas_Controller

    constructor {view} {
	
	foreach state {showItemLabels itemLabel} {
	    set ${state}_var ""
	}
	
	next $view
	
    }

    method init {} {
	my variable canvas
	
	next

	$canvas bind "layer" "<Enter>" "+[self namespace]::my PendingItemLabel"
	$canvas bind "layer" "<Leave>" "+::loon::loonlabel_cancel"

	bind $canvas <Leave> "+::loon::loonlabel_cancel"
	
    }


    method setModel {Model} {
	
	my variable model
	
	set ns [info object namespace $Model] 
	foreach state {showItemLabels itemLabel} {
	    set ${state}_var [uplevel #0 [list ${ns}::my varname $state]]
	}       	
	
	next $Model
    }


    method PendingItemLabel {} {
	my variable model canvas
	if {$model ne "" && [set $showItemLabels_var]} {
	    set tags [$canvas gettags current]
	    set layer [lindex $tags 1]
	    set i [string range [lindex $tags 3] 4 end]

	    if {$layer eq "model"} {
		set label [lindex [set $itemLabel_var] $i]
	    } else {
		set label [lindex [set [$model layer getObject $layer]::itemLabel] $i]
	    }
	    
	    ::loon::loonlabel_pending canvas $canvas $label
	}
    }

}
