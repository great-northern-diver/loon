
::oo::class create ::loon::classes::withStateBindings {
    
    superclass ::loon::classes::Configurable\
	::loon::classes::Bindable
    
    variable systemStateBindings userStateBindings

    constructor {args} {

        my variable inConfiguration

	set systemStateBindings [::loon::classes::StateBindings new\
				     "stateSystembinding" FALSE]
	set userStateBindings [::loon::classes::StateBindings new\
				   "stateBinding" TRUE [my varname inConfiguration]]
	
	next {*}$args
	
    }

    destructor {
	$systemStateBindings notify [self] destroy
	$userStateBindings notify [self] destroy
	$systemStateBindings destroy
	$userStateBindings destroy
	
	next
	
    }
    
    method InfoDebug {args} {
	next systemStateBindings userStateBindings {*}$args
    }

    
    method BindState {systemOrUser cmd args} {
	switch -- $cmd {
	    remove {
		[set ${systemOrUser}StateBindings] delete {*}$args
	    }
	    delete -
	    reorder -
	    get -
	    add {
		[set ${systemOrUser}StateBindings] $cmd {*}$args
	    }
	    ids {
		[set ${systemOrUser}StateBindings] get
	    }
	    notify {
		error "notify bindings are not possible."
	    }
	    default {
		puts "bind $cmd state, use add, [info level 0] & [info level -1]"
		[set ${systemOrUser}StateBindings] add $cmd {*}$args
	    }		    
	}
    } 

    method ApplyConfigure {} {
	
	next

	my variable changedStates confDict inConfiguration
	if {[llength $changedStates] > 0} {
	    ## ApplyConfigure Post withScatterplotBindings
	    ##   - notify state bindings
	    
	    #set events [dict keys $confDict init_*]
	    set events $changedStates
	    
	    $systemStateBindings notify [self] $events
	    $userStateBindings notify [self] $events
	}	
    }

    method SetSubstitutions {subst} {
	[info object namespace $systemStateBindings]::my setSubstitutions $subst
	[info object namespace $userStateBindings]::my setSubstitutions $subst
	next $subst
    }

    method AddSubstitutions {subst} {
	[info object namespace $systemStateBindings]::my addSubstitutions $subst
	[info object namespace $userStateBindings]::my addSubstitutions $subst
	next $subst
    }
    

}
