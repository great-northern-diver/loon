

::oo::class create ::loon::classes::Itemlabel_Controller {

    variable showItemlabels_var itemlabel_var

    superclass ::loon::classes::Canvas_Controller

    constructor {view} {
	
	foreach state {showItemlabels itemlabel} {
	    set ${state}_var ""
	}
	
	next $view
	
    }

    method init {} {
	my variable canvas
	
	next

	$canvas bind "layer" "<Enter>" "+[self namespace]::my PendingItemlabel"
	$canvas bind "layer" "<Leave>" "+::loon::loonlabel_cancel"

	bind $canvas <Leave> "+::loon::loonlabel_cancel"
	
    }


    method setModel {Model} {
	
	my variable model
	
	set ns [info object namespace $Model] 
	foreach state {showItemlabels itemlabel} {
	    set ${state}_var [uplevel #0 ${ns}::my varname $state]
	}       	
	
	next $Model
    }


    method PendingItemlabel {} {
	my variable model canvas
	if {$model ne "" && [set $showItemlabels_var]} {
	    set tags [$canvas gettags current]
	    set layer [lindex $tags 1]
	    set i [string range [lindex $tags 3] 4 end]

	    if {$layer eq "model"} {
		set label [lindex [set $itemlabel_var] $i]
	    } else {
		set label [lindex [set [$model layer getObject $layer]::itemlabel] $i]
	    }
	    
	    ::loon::loonlabel_pending canvas $canvas $label
	}
    }

}
