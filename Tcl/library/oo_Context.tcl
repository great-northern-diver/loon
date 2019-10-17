
oo::class create ::loon::classes::Context {
    
    superclass ::loon::classes::Configurable
    
    
    variable  graph navigator navigatorBinding navigatorObj substitutions
    
    constructor {Graph Navigator} {

	set graph $Graph
	set navigator $Navigator
	set navigatorBinding "" 
	set navigatorObj ""
	
	set substitutions [list %W $graph %nav $navigator]

	set navigatorObj [uplevel #0 [list $graph navigator getObject $navigator]]
	set navigatorBinding [$navigatorObj systembind state add all\
				  "[self namespace]::my NavigatorUpdate"]
	
	next
	my New_state separator string 1 ":"
	my New_state command string 1 ""

	my SetStateDescription separator\
	    "string used to separate the graph node names into spaces"

	my SetStateDescription command\
	    "callback that is evaluated at every navigator location change"
	
    }

    method EvalConfigure {} {
	my variable confDict

	next

	if {[dict exists $confDict separator]} {
	    if {[regexp " " [dict get $confDict separator]]} {
		error "separator can not contain spaces."
	    }
	}
    }
    
    
    method InitNavigator {} {}

    method NavigatorUpdate {} {}

    method EvalCommand {args} {

	my variable command
	if {$command ne ""} {
	    uplevel #0 [list $command {*}$args]
	}
    }

    method AddSubstitution {key value} {
	lappend substitutions $key $value
    }
    
}
