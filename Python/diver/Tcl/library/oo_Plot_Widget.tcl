
::oo::class create ::loon::classes::Plot_Widget {
    
    superclass ::loon::classes::Widget
    
    method aspect {args} {
	my variable widgetview
	$widgetview aspect {*}$args
    }
}
