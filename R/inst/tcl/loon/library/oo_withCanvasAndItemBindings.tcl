

oo::class create loon::classes::withCanvasAndItemBindings {
    
    superclass ::loon::classes::Bindable
    
    
    variable userItemBindings systemItemBindings\
	userCanvasBindings systemCanvasBindings

    constructor {args} {
	
	set userItemBindings [::loon::classes::ItemBindings new "itemBinding" TRUE]
	set systemItemBindings [::loon::classes::ItemBindings new "itemSystembinding" FALSE]
	
	set userCanvasBindings [::loon::classes::CanvasBindings new "canvasBinding" TRUE]
	set systemCanvasBindings [::loon::classes::CanvasBindings new "canvasSystembinding" FALSE]
	
	next {*}$args
	
    }

    method Make {args} {
	next {*}$args

	my variable canvas map
	foreach b {userItemBindings systemItemBindings\
		       userCanvasBindings systemCanvasBindings} {
	    [set $b] setCanvas $canvas
	    [set $b] setMap $map
	}
    }
    
    method InfoDebug {args} {
	next userItemBindings systemItemBindings\
	    userCanvasBindings systemCanvasBindings {*}$args
    }
    
    
    method BindItem {systemOrUser cmd args} {
	switch -- $cmd {
	    ids {
		[set ${systemOrUser}ItemBindings] get
	    }
	    delete -
	    reorder -
	    get -
	    add {
		[set ${systemOrUser}ItemBindings] $cmd {*}$args
	    }
	    notify {
		error "notify bindings are not possible."
	    }
	    default {
		puts "bind $cmd, use add, [info level 0] & [info level -1]"
		[set ${systemOrUser}ItemBindings] add $cmd {*}$args
	    }		    
	}
    }

    method BindCanvas {systemOrUser cmd args} {
	switch -- $cmd {
	    ids {
		[set ${systemOrUser}CanvasBindings] get
	    }
	    delete -
	    reorder -
	    get -
	    add {
		[set ${systemOrUser}CanvasBindings] $cmd {*}$args
	    }
	    notify {
		error "notify bindings are not possible."
	    }
	    default {
		puts "bind $cmd, use add, [info level 0] & [info level -1]"
		[set ${systemOrUser}CanvasBindings] add $cmd {*}$args
	    }		    
	}
    }
    
    method SetSubstitutions {subst} {
	$userItemBindings setSubstitutions $subst
	$systemItemBindings setSubstitutions $subst
	$userCanvasBindings setSubstitutions $subst
	$systemCanvasBindings setSubstitutions $subst
    }

    method AddSubstitutions {subst} {
	$userItemBindings addSubstitutions $subst
	$systemItemBindings addSubstitutions $subst
	$userCanvasBindings addSubstitutions $subst
	$systemCanvasBindings addSubstitutions $subst
    }

    method currenttags {} {
	my variable canvas
	return [lrange [$canvas gettags current] 0 end-1]	
    }

    method currentindex {} {
	my variable canvas
	return [string range [lindex [lrange [$canvas gettags current] 0 end-1] 3] 4 end]
    }
}
