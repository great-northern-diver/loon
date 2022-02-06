

::oo::class create ::loon::classes::withContexts {

    superclass ::loon::classes::fancyInfo

    variable contextObjects


    constructor {args} {
	
	set contextObjects [::loon::classes::LabelledCollection new\
				  "Context" "context" "context" "%con"]
	
	next {*}$args
	
    }

    method InfoDebug {args} {
	next contextObjects {*}$args
    }

    method context {subcommand args} {
	
	switch -- $subcommand {	    
	    add {
		return [my AddContext {*}$args]
	    }
	    ids {
		$contextObjects list {*}$args
	    }
	    list {
		puts "use context ids instead of context list"
		$contextObjects $subcommand {*}$args
	    }
	    use -
	    with -
	    target -
	    delete -
	    getObject -
	    getLabel -
	    getType -
	    relabel {
		$contextObjects $subcommand {*}$args
	    }
	    default {
		error "\"$subcommand\" is not a valid subcommand for context."
	    }
	}
    }
    
    method AddContext {type args} {
	my variable graph navigatorId

	set id [$contextObjects nextId]
	
	set createNewPlot FALSE
	
	switch -- $type {
	    geodesic2d {
		if {"-command" ni $args} {
		    set gr [regsub "^::" $graph ""]
		    set p [::loon::plot] 
		    lappend args -command\
			"$p configure -x %x -y %y -xlabel %xlabel -ylabel %ylabel"
		    set createNewPlot TRUE
		}
		
		set obj [::loon::classes::Geodesic2d new $graph $navigatorId]
		
	    }
	    slicing2d {

		set obj [::loon::classes::Slicing2d new $graph $navigatorId]
		if {"-command" ni $args} {
		    [info object namespace $obj]::my CreateDisplays
		    lappend args -command "$obj updateDisplays"
		}
		
	    }
	    context2d {
		set obj [::loon::classes::Context2d new $graph $navigatorId]
	    }
	    default {
		"unkown context: \"$type\"."
	    }
	}
	
	set label [::loon::listfns::extractAndRemoveOption args -label ""]
	if {[llength $args] > 0} {
	    $obj configure {*}$args
	}
	
	[info object namespace $obj]::my InitNavigator 
	
	if {$createNewPlot} {
	    $p scaleto world
	}

	[info object namespace $obj]::my AddSubstitution %con $id
	
	$contextObjects add $obj $label $type
	
	return $id
    }
    
    method BindContext {systemOrUser cmd args} {
	$contextObjects bind $systemOrUser $cmd {*}$args
    }

    method SetSubstitutions {subst} {
	$contextObjects setSubstitutions $subst
	next $subst
    }

    method AddSubstitutions {subst} {
	$contextObjects addSubstitutions $subst
	next $subst
    }


}
