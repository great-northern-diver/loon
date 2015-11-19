
::oo::class create ::loon::classes::Graph_Widget {
    
    superclass ::loon::classes::Plot_Widget\
	::loon::classes::Graph_Model
    
    method InitView {} {
	my variable widgetpath widgetview
	
	set widgetview [::loon::classes::Graph_View new $widgetpath]
	
	next
    }

    

}
