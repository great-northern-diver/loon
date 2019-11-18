## This is an attempt to make the inspector simpler
## with now model view...


::oo::class create ::loon::classes::Inspector2 {
    
    superclass ::loon::classes::withStateBindings

    variable path activewidget widgetBinding bindingType bindingSubstitution
    
    constructor {Path {BindingType "state"} {BindingSubstitution ""}} {

	set path $Path
	set activewidget ""
	set widgetBinding ""
	set bindingType $BindingType
	set bindingSubstitution $BindingSubstitution

	my Make
	next
	
	my SetSubstitutions [list %W $path]
	
	my AddStates activewidget
    }
    
    method InfoDebug {args} {
	next path activewidget widgetBinding {*}$args
    }
    
    
    method Make {} {
	frame $path -class LoonInspector
    }
    
    
    method EvalConfigure {} {
	my variable confDict
	
	next 
	
	if {[dict get $confDict has_activewidget]} {
	    set arg_widget [dict get $confDict arg_activewidget]
	    if {$arg_widget ne ""} {
		my CheckNewActivewidget $arg_widget  
	    }
	    dict set confDict new_activewidget $arg_widget
	}
	
    }
    
    method CheckNewActivewidget {widget} {
	set class [info object class $widget]
	puts "Implement CheckNewActivewidget method for class $class"
    }
    

    method ActivewidgetChanged {events args} {
	if {$events eq "destroy"} {
	    my configure -activewidget ""
	} else {
	    my ActivewidgetEvents $events {*}$args
	}
    }

    method ActivewidgetEvents {events} {
	set class [info object class [self]]
	puts "Implement ActivewidgetEvents method for class $class"
    }

    method UnregisterActivewidget {} {
	if {$widgetBinding ne ""} {
	    catch {uplevel #0 [list $activewidget systembind $bindingType delete $widgetBinding]}
	    set widgetBinding ""
	}
    }

    method RegisterActivewidget {} {
	if {$activewidget ne "" && $bindingType ne ""} {
	    set widgetBinding\
		[uplevel #0 [list $activewidget systembind $bindingType add all\
				 "[self namespace]::my ActivewidgetChanged %e $bindingSubstitution"]]
	}
    }
    
    destructor {
	catch {my UnregisterActivewidget}
	next
    }
    
    method ApplyConfigure {} {
	my variable confDict
	if {[dict exists $confDict new_activewidget]} {
	    my UnregisterActivewidget
	}
	next
    }
    
    method HookAfterStatesSet {} {
	my variable changedStates 
	if {"activewidget" in $changedStates} {
	    my RegisterActivewidget
	}
	next
    }
}
