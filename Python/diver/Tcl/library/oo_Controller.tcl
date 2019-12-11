

::oo::class create ::loon::classes::Controller {
    
    superclass ::loon::classes::fancyInfo

    
    variable model view
    
    constructor {View} {

	## The model can be changed dynamiclly, the view not...
	set model ""
	set view $View
	
	my init
    }

    method setModel {Model} {
	set model $Model
    }
    
    method init {} {}
    
    
}
