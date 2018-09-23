

::oo::class create ::loon::classes::Configurable {

    superclass ::loon::classes::fancyInfo

    variable configurableOptions confDict changedStates\
	inConfiguration\
	managedStates stateDescriptions stateInfo\
	eval_configure_cmds

    ## The Configure class is the mother of all _Model (ang glyph,
    ## etc...) classes most importantly it provides the configure and
    ## cget subcommand.
    ##
    ## The configure is now fairly involved
    ##  -
    ##
    ##
    ##

    constructor {args} {

	set configurableOptions ""  ;# loon language > states
	set confDict ""             ;# Dictionary with all configure changes
	set changedStates ""        ;#list of all states that\
	                             #got modified in configure call

	set inConfiguration FALSE

	## API allows to have classes manage some of their states
	set managedStates [dict create]  ;# states added with New_state


	set eval_configure_cmds {}  ;# commands to evaluate in EvalConfigure

	## State descriptions when running info states
	set stateDescriptions [dict create]
	set stateInfo [dict create]

	next {*}$args

    }

    method InfoDebug {args} {
	next {*}[dict keys $managedStates] {*}$args
    }


    method config {args} {my configure {*}$args}

    method configure {args} {

	if {$inConfiguration && $::loon::Options(printInConfigurationWarning)} {
            set argNames {}
            foreach {aname aval} $args {
                if {[llength $aname] eq 1} {
                    if {[string range $aname 0 0] eq "-"} {
                        lappend argNames [string range $aname 1 end]
                    }
                }
            }
	    ::loon::warning\
                [format "%s is already in configuration\n states intended to configure: %s\n turn this message off with %s"\
                     [self] [join $argNames ", "] $::loon::Options(printInConfigurationWarningMsg)]
	}

	set inConfiguration TRUE

	set had_error [catch {
	    if {[llength $args] eq "0"} {
		error [format "Configurable options (states) are: %s"\
			   [join [lmap s $configurableOptions {format -%s $s}] ", "]]
	    }


	    set confDict [dict create]

	    if {[expr {[llength $args] % 2}]} {
		error "configure needs key value pairs. Arguments are: $args"
	    }

	    ## create has_(configureOption) and arg_(...)
	    set argList {}
	    foreach {key value} $args {
		if {[string range $key 0 0] != "-"} {
		    error "state \"$key\" is not valid. Only \"-state value\" pairs are accepted."
		}

		set option [string replace $key 0 0]
		if {$option ni $configurableOptions} {
		    if {[dict exists $::loon::Options(abbreviations) $option]} {
			puts [format "warning: please do not use state abbreviations,\
                               i.e. use \"%s\" instead of \"%s\"."\
				  [dict get $::loon::Options(abbreviations) $option]\
				  $option]
			set option [dict get $::loon::Options(abbreviations) $option]
		    } else {
			error "state \"$key\" not valid."
		    }
		}

		lappend argList $option

		## create has_option and arg_option
		dict set confDict [format "has_%s" $option] TRUE
		dict set confDict [format "arg_%s" $option] $value
	    }

	    foreach s $configurableOptions {
		if {$s ni $argList} {
		    dict set confDict [format "has_%s" $s] FALSE
		}
	    }

	    my EvalConfigureInit

	    my EvalConfigure

	    my ApplyConfigure
	} error_msg error_info]

	set inConfiguration FALSE

	if {$had_error} {
	    if {$::loon::Options(printConfigureDebug)} {
		#puts "configure args: $args"
		puts [dict get $error_info -errorinfo]
	    }
	    error "$error_msg"
	}

    }


    method EvalConfigureInit {} {
	# puts  "EvalConfigureInit ----- isConfigurable"
    }

    method EvalConfigure {} {
	## Check managed states
	## [list my Check $type $name $dim {*}$args]
	foreach cmd $eval_configure_cmds {
	    eval $cmd
	}

    }

    method cget {option} {
	## right now can get any member variable

	set opt [string range $option 1 end]
	if {[catch {my variable $opt; set out [set $opt] }]} {
	    error [format "\"$opt\" is not a valid option.\
                   Valid configurable options are: %s"\
		       [join [lmap s $configurableOptions {format -%s $s}] ", "]]
	} else {
	    if {$opt eq "color" && [llength $out] eq 1} {
		## remove the {}
		set out [lindex $out 0]
	    }
	    return $out
	}

    }

    method ApplyConfigure {} {
	#puts "--- Configure apply changes [self]:"
	#puts "---\n\n"
	#
	#puts $confDict

	#puts "---\n\n"



	set changedStates {}
	foreach key [dict keys $confDict new_*] {
	    set state [string range $key 4 end]
	    my variable $state
	    set stateValue [dict get $confDict $key]
	    if {$stateValue ne [set $state]} {
		#puts "$key: [lrange $stateValue 0 6]"
		#puts " apply configure: $state: $stateValue"
		lappend changedStates $state
		set $state $stateValue
	    }
	}
	#set inConfiguration FALSE

	## to use if need to make some state modifications before
	## change bindings get sent
	my HookAfterStatesSet
	#puts "---"
    }

    method HookAfterStatesSet {} {}

    ## Add a new state
    method New_state {name type dim init args} {
	## create variable
	my variable $name

	if {[string is integer $dim] || $dim eq "any"} {
	    set $name [my Check_$type $name $init $dim {*}$args]
	}
	## VariableDimensions will handle the rest

	dict set managedStates $name\
	    [dict create type $type dim $dim init $init args $args]

	my AddStates $name

	lappend eval_configure_cmds [list my Check $type $name $dim {*}$args]

    }

    method AddStates {args} {
	lappend configurableOptions {*}$args
    }

    method SetStateDescription {state description} {
	dict set stateDescriptions $state $description
    }

    method SetStateInfo {state type dim init} {
	dict set stateInfo $state\
	    [dict create type $type\
		 dim $dim init $init]
    }


}
