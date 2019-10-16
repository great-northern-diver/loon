
::oo::class create ::loon::classes::Histogram_Widget {
    
    superclass ::loon::classes::Plot_Widget\
	::loon::classes::Histogram_Model


    method InitView {} {
	my variable widgetpath widgetview

	set widgetview [::loon::classes::Histogram_View new $widgetpath]
	
	next
	
    }

    method aspect {args} {
	my variable widgetview
	$widgetview aspect {*}$args
    }
}
