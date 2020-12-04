
oo::class create ::loon::classes::Serialaxes {

    superclass ::loon::classes::withStateBindings\
	::loon::classes::VariableDimensions


    variable axesLabels data sequence scaled_data numeric_data glyphs\
	axesLabelsForSequence

    constructor {args} {

	next {*}$args

	my New_state showAxes boolean 1 TRUE
	my New_state showAxesLabels boolean 1 TRUE

	my New_state linewidth positive_double n 1

	# default length.out
	# if we set it as 20, the lines are too bumpier
	# if we set it as 100, the computation time would be too long
	# if you can speed the code, try to increase this value

	my New_state andrewsSeriesLength positive_double 1 40

	my New_state scaling factor 1 variable\
	    [list "variable" "data" "observation" "none"]

	my New_state axesLayout factor 1 radial {"radial" "parallel"}

	my New_state showArea boolean 1 FALSE

	my New_state  andrews boolean 1 FALSE

	my SetStateInfo data data n ""

	set axesLabels {}
	set axesLabelsForSequence {}
	set data {}
	set scaled_data {}
	set numeric_data {}
	set sequence {}
	set glyphs {}

	my AddStates axesLabels data sequence

	my SetStateDescription showAxes\
	    "boolean to specify whether to display the axes or not"
	my SetStateDescription showAxesLabels\
	    "boolean to specify whether to display axes labels and title or not"
    my SetStateDescription andrews\
	    "boolean to specify whether to do the Fourier transformation"
	my SetStateDescription linewidth\
	    "linewidth of glyphs"
	my SetStateDescription scaling\
	    "one of 'variable', 'data', 'observation' or 'none' to specify how the data is scaled. See web-documentation."
	my SetStateDescription axesLayout\
	    "either 'radial' for stacked star glyphs or 'parallel' for parallel coordinates plot"
	my SetStateDescription showArea\
	    "boolean to specify whether to fill the glyph or not"
	my SetStateDescription data\
	    "data set used for glyphs"
    }

    method InfoDebug {args} {
	next axesLabels sequence {*}$args

    }

    method EvalConfigureInit {} {
	my variable confDict n

	## Need initialize
	if {[dict get $confDict has_data]} {

	    set arg_data [dict get $confDict arg_data]

	    ## Check if data is valid
	    set n_tmp ""
	    dict for {name variable} $arg_data {
		if {$n_tmp eq ""} {
		    set n_tmp [llength $variable]
		} else {
		    set n_var [llength $variable]
		    if {$n_var ne $n_tmp} {
			"-data has variable \"$variable\" with length \"$n_var\".\
                          However length $n_tmp is expected."
		    }
		}
	    }
	    dict set confDict init_n TRUE
	    dict set confDict new_n $n_tmp
	    dict set confDict new_data $arg_data
	    dict set confDict new_sequence [dict keys $arg_data]
	    dict set confDict new_axesLabels [dict keys $arg_data]
	}

	next
    }


    method EvalConfigure {} {
	my variable confDict
	next

	if {[dict get $confDict has_sequence]} {

	    set arg_sequence [dict get $confDict arg_sequence]
	    if {[dict exists $confDict init_n] && [dict get $confDict init_n]} {
		set varnames [dict keys [dict get $confDict new_data]]
	    } else {
		set varnames [dict keys $data]
	    }
	    foreach s $arg_sequence {
		if {$s ni $varnames} {
		    error "-sequence element \"$v\" is not a valid variable name.\
                           Valid names are: $varnames"
		}
	    }
	    dict set confDict new_sequence $arg_sequence
	}

	if {[dict get $confDict has_axesLabels]} {
	    if {[dict exists $confDict new_data]} {
		set n_var [dict size [dict get $confDict new_data]]
	    } else {
		set n_var [dict size $data]
	    }
	    set arg_axesLabels [dict get $confDict arg_axesLabels]
	    set n_arg [llength $arg_axesLabels]
	    if {$n_var ne $n_arg} {
		error "-axesLabels has length $n_arg but length $n_var is expected."
	    }
	    dict set confDict new_axesLabels $arg_axesLabels
	}



    }

    method HookAfterStatesSet {} {

	my variable changedStates scaling

	set has_new_data [expr {"data" in $changedStates}]
	set has_new_scaling [expr {"scaling" in $changedStates}]
	set has_new_sequence [expr {"sequence" in $changedStates}]

	if {$has_new_data || $has_new_scaling} {
	    if {$data eq ""} {
		set numeric_data {}
		set scaled_data {}
		set axesLabelsForSequence {}
	    } else {

		if {$has_new_data} {
		    set numeric_data $data
		    foreach var [dict keys $numeric_data] {
			if {![::loon::listfns::isNumeric [dict get $numeric_data $var]]} {
			    dict set numeric_data $var\
				[::loon::listfns::toNumeric [dict get $numeric_data $var]]
			}
		    }
		    set glyphs {}
		}

		switch -- $scaling {
		    "none" {
			set scaled_data $numeric_data
		    }
		    "data" {
			set min {}
			set max {}
			dict for {key value} $numeric_data {
			    set mm [::loon::listfns::MinMax $value]
			    lappend min [lindex $mm 0]
			    lappend max [lindex $mm 1]
			}

			set min [::loon::listfns::min $min]
			set max [::loon::listfns::max $max]

			set scaled_data [dict create]
			dict for {key value} $numeric_data {
			    dict set scaled_data $key [::loon::listfns::scale01 $value $min $max]
			}
		    }
		    "variable" {
			set scaled_data [dict create]
			dict for {key value} $numeric_data {
			    dict set scaled_data $key [::loon::listfns::scale01 $value]
			}
		    }
		    "observation" {

			set scaled_data [dict create]
			my variable n

			set keys [dict keys $numeric_data]

			for {set i 0} {$i < $n} {incr i} {
			    set obs {}
			    foreach key $keys {
				lappend obs [lindex [dict get $numeric_data $key] $i]
			    }

			    set sobs [::loon::listfns::scale01 $obs]

			    foreach key $keys o $sobs {
				dict lappend scaled_data $key $o
			    }
			} ;# for
		    } ;# observation
		} ;# switch
	    } ;# data ne ""
	} ;# data || scaling

	if {$has_new_data || $has_new_sequence} {
	    set axesLabelsForSequence {}
	    if {$data ne ""} {
		set varnames [dict keys $data]
		foreach s $sequence {
		    set i [lsearch -exact $varnames $s]
		    if {$i ne "-1"} {
			lappend axesLabelsForSequence [lindex $axesLabels $i]
		    } else {
			lappend axesLabelsForSequence "??"
		    }
		}
	    }
	}

	if {$has_new_data || $has_new_scaling || $has_new_sequence} {
	    my CreateGlyphsList
	}

	next
    }

    method CreateGlyphsList {} {
	my variable n sequence
	#puts "Create Glyphs List: $sequence"
	set glyphs ""
	if {$data eq "" || [llength $sequence] eq 0} {
	    return
	}

	for {set i 0} {$i < $n} {incr i} {
	    set lst {}
	    foreach var $sequence {
		lappend lst [lindex [dict get $scaled_data $var] $i]
	    }
	    lappend glyphs $lst
	}
    }
}
