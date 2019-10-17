::oo::class create ::loon::classes::Widget {
    
    superclass loon::classes::fancyInfo

    
    variable widgetpath widgetview

    constructor {Path} {

	set widgetpath $Path
	
	next 

	my InitView
    }
    
    destructor {
	next	
    }

    method InfoDebug {args} {
	next widgetpath widgetview {*}$args
    }
    
    method InitView {} {
	
	if {$widgetview eq ""} {
	    error "create widget view first"
	}

	uplevel #0 rename $widgetpath ${widgetpath}.fr
	uplevel #0 rename [self] $widgetpath
	
	## after renaming
	$widgetview setPlotModel [self]

	my AddSubstitutions [list %W $widgetpath]
	## also add the to the canvas bindings
    }

    method BindCanvas {args} {
	my variable widgetview
	[info object namespace $widgetview]::my BindCanvas {*}$args
    }
    
    method BindItem {args} {
	my variable widgetview
	[info object namespace $widgetview]::my BindItem {*}$args
    }
    
    method currenttags {} {
	my variable widgetview
	return [[info object namespace $widgetview]::my currenttags]
    }
    
    method currentindex {} {
	my variable widgetview
	return [[info object namespace $widgetview]::my currentindex]
    }

    
    method AddSubstitutions {subst} {
	my variable widgetview
	[info object namespace $widgetview]::my AddSubstitutions $subst
	next $subst
    }
    
    method SetSubstitutions {subst} {
	my variable widgetview
	[info object namespace $widgetview]::my SetSubstitutions $subst
	next $subst
    }
    
    method isWidget {} {
	return TRUE
    }

    method redraw {} {
	$widgetview redraw
    }
}
