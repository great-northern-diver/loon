
## Check Routines For Configurable class

::oo::define ::loon::classes::Configurable {
    ## var  : varname
    ## type : boolean
    ##        color
    ##        positive_double
    ##        double
    ##        factor
    ##        string
    ## dim  : 0,1,2, any (class VariableDimensions: n, p, q, ...)

    ## add to confDict
    method Check {type var dim args} {
	if {[dict get $confDict has_$var]} {

	    set arg_var [dict get $confDict arg_$var]
	    set n_var [llength $arg_var]

	    dict set confDict new_$var\
		[my Check_$type $var $arg_var $dim {*}$args]
	}
    }


    ## VariableDimensions implements this method too..
    method Check_Dimensionality {var values dim} {

	if {$dim eq "any"} {
	    ## do nothing
	} elseif {[string is integer $dim]} {
	    if {[llength $values] ne $dim} {
		error "-$var has length [llength $values] but length $dim is expected."
	    }
	} else {
	    error "dim \"$dim\" not known."
	}
	return $values
    }

    method Check_boolean {var values dim args} {

	if {![::loon::listfns::isBoolean $values]} {
	    error "Not all elements in -$var are of type boolean."
	}

	return [my Check_Dimensionality\
		    $var [::loon::listfns::booleanAsTRUEFALSE $values] $dim]
    }

    method Check_color {var values dim args} {


	## remove alpha part from hex6 string
	set col {}
	foreach c $values {

	    ## remove unexpected braces
        set c [regsub -all {\{|\}} $c ""]

	    if {[string index $c 0] eq "#" && [string length $c] eq 9} {
		lappend col [string range $c 0 6]
	    } else {
		lappend col $c
	    }
	}
	set values $col


	if {![::loon::listfns::isColor $values]} {
	    #puts "Warning: Non color arguments for a color state is treated as factor."
	    set values [::loon::listfns::mapColor $values]
	}

	## Careful maybe lrepeat $n $color must be lrepeat $n {*}$color
	## because of autolisting first element

	set values [my Check_Dimensionality\
			$var [::loon::listfns::toHexcolor $values] $dim]

	#if {[llength $values] eq 1} {
	#    set values {*}$values
	#}

	return $values
    }

    method Check_colorOrTransparent {var values dim args} {


	## if all transparent
	if {[llength $values] eq 0} {
	    if {$dim ne "any" && $dim ne 1} {
		my variable $dim
		set values [lrepeat [set $dim] ""]
	    } else {
		set values ""
	    }
	    return $values
	}


	foreach val $values {
	    if {$val ne ""} {
		if {![::loon::listfns::isColor $val]} {
		    ##puts "Warning: Non color arguments for a color\
		    ##state is treated as factor."
		    set values [::loon::listfns::mapColor $values]
		    break
		}
	    }
	}

	if {[llength $values] eq 1} {
	    set values {*}[::loon::listfns::toHexcolor $values]
	} else {
	    set i 0
	    foreach val $values {
		if {$val ne ""} {
		    lset values $i [::loon::listfns::toHexcolor $val]
		}
		incr i
	    }
	}

	set values [my Check_Dimensionality $var $values $dim]


	## unwrap color {#...}
	if {[llength $values] eq 1} {
	    set values {*}$values
	}

	return $values
    }

    method Check_positive_double {var values dim args} {

	foreach x $values {
	    if {![string is double $x] || $x <= 0} {
		error "Not all elements in -$var are of type positive double (e.g. $x)."
	    }
	}

	return [my Check_Dimensionality $var $values $dim]
    }

    ## In Tcl everything is a string
    ## Length 1 is not checked
    ## length 2+ must be {{Hello World} {This Is A} {Test}}
    method Check_string {var values dim args} {
	if {$dim ne "any" && [string is integer $dim]} {
	    if {$dim > 1} {
		if {[llength $values] ne $dim} {
		    error "-$var has length [llength $values] but length $dim is expected."
		}
	    }
	} else {
	    set values [my Check_Dimensionality $var $values $dim]
	}
	return $values
    }

    method Check_double {var values dim args} {

	if {![::loon::listfns::isNumeric $values]} {
	    error "Not all elements in -$var are of type double."
	}

	return [my Check_Dimensionality $var $values $dim]
    }

    method Check_factor {var values dim args} {

	set factors [lindex $args 0]

	foreach v $values {
	    if {$v ni $factors} {
		error [format "factor -$var level \"$v\" is not valid. Choose from: %s."\
			   [join $factors ", "]]
	    }
	}

	return [my Check_Dimensionality $var $values $dim]
    }

    method Check_integer  {var values dim args} {
	if {![::loon::listfns::isInteger $values]} {
	    error "Not all elements in -$var are of type integer."
	}

	return [my Check_Dimensionality $var $values $dim]
    }


    method Check_positive_integer  {var values dim args} {
	if {![::loon::listfns::isPositiveInteger $values]} {
	    error "Not all elements in -$var are of type positive integer."
	}

	return [my Check_Dimensionality $var $values $dim]
    }


    ## tempcoords are either empty or double of length dim
    method Check_tempcoords {var values dim args} {

	if {[llength $values] eq 0} {
	    return {}
	} else {
	    my Check_double $var $values $dim {*}$args
	}

    }


    method Check_nested_double {var values dim args} {

	foreach vals $values {
	    if {![::loon::listfns::isNumeric $vals]} {
		error "Not all elements in -$var are of type double."
	    }

	}

	return [my Check_Dimensionality $var $values $dim]
    }

    method Check_nested_positive_double {var values dim args} {

	foreach vals $values {
	    if {![::loon::listfns::isNumeric $vals] || } {
		error "Not all elements in -$var are of type double."
	    }
	    if {![::loon::listfns::isPositiveNumber $vals]} {
		error "Not all elements in -$var are positive."
	    }
	}

	return [my Check_Dimensionality $var $values $dim]
    }


    method Check_image {var values dim args} {

	if {![::loon::listfns::isImage $values]} {
	    error "Not all elements in -$var are valid images (e.g. in \[image names\])."
	}

	return [my Check_Dimensionality $var $values $dim]
    }
}
