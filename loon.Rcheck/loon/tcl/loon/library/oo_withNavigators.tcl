

::oo::class create ::loon::classes::withNavigators {

    superclass ::loon::classes::fancyInfo

    variable navigatorObjects activeNavigator


    constructor {args} {
	
	set navigatorObjects [::loon::classes::LabelledCollection new\
				  "Navigator" "navigator" "navigator" "%nav"]


	set activeNavigator ""


	next {*}$args

	my AddStates "activeNavigator"
	my AddStatesToVariableDimension "activeNavigator" "0||1"
	
    }

    method InfoDebug {args} {
	next navigatorObjects {*}$args
    }

    method navigator {subcommand args} {
	
	switch -- $subcommand {	    
	    add {
		return [my AddNavigator {*}$args]
	    }
	    ids {
		$navigatorObjects list {*}$args
	    }
	    list {
		puts "use navigator ids instead of navigator list"
		$navigatorObjects $subcommand {*}$args
	    }
	    use -
	    with -
	    target -
	    get -
	    ids -
	    delete -
	    getObject -
	    getLabel -
	    getType -
	    relabel {
		$navigatorObjects $subcommand {*}$args
	    }

	    default {
		error "\"$subcommand\" is not a valid subcommand for navigator."
	    }
	}
    }


    method EvalConfigure {} {
	my variable confDict

	next

	if {[dict get $confDict has_activeNavigator]} {
	    set arg_activeNavigator [dict get $confDict arg_activeNavigator]

	    if {[llength $arg_activeNavigator] ne 0} {
		if {$arg_activeNavigator ni [$navigatorObjects list]} {
		    error "activeNavigator \"$activeNavigator\" is not a valid navigator"
		}
	    }
	    dict set confDict new_activeNavigator $arg_activeNavigator
	}
    }
    
    method AddNavigator {args} {
	

	set id [$navigatorObjects nextId]

	set navObj [::loon::classes::Navigator new [self] $id]
	
	set label [::loon::listfns::extractAndRemoveOption args -label ""]
	if {[llength $args] > 0} {
	    $navObj configure {*}$args
	}
	

	
	## Now add subsitutions
	if {[my isWidget]} {
	    my variable widgetpath
	    set subst [list %W $widgetpath %nav $id]
	} else {
	    set subst [list %nav $id]
	}
	[info object namespace $navObj]::my AddSubstitutions $subst
	
	$navigatorObjects add $navObj $label navigator
	
	return $id
    }
    
    method HookAfterStatesSet {} {
	my variable changedStates
	
	## If any of the Bullets is not on a visible edge or node move
	## the bullet to the first node in the nodes state
	
	next
    } 
    
    method BindNavigator {systemOrUser cmd args} {
	$navigatorObjects bind $systemOrUser $cmd {*}$args
    }

    method SetSubstitutions {subst} {
	$navigatorObjects setSubstitutions $subst
	next $subst
    }

    method AddSubstitutions {subst} {
	$navigatorObjects addSubstitutions $subst
	next $subst
    }


}
    
