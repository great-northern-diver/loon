
::oo::class create ::loon::classes::Worldview {
    
    superclass ::loon::classes::Inspector2
    
    variable view
    
    constructor {Path} {
	next $Path

	set view [::loon::classes::Worldview_View new $Path]
	
    }

    method Make {} {
	## Do nothing, as the view creates the widget
    }
    
    method CheckNewActivewidget {widget} {
	if {![info object isa typeof $widget "::loon::classes::Widget"]} {
	    error "-activewidget needs to be a loon widget."
	}
    }

    
    method RegisterActivewidget {} {
	my variable activewidget
	$view setPlotModel $activewidget
	next 
    }

    
    method ActivewidgetEvents {events} {
	if {"destroy" in $events} {
	    $view setPlotModel ""
	}
    }

    

}
