
::oo::class create ::loon::classes::withGlyphs {

    superclass ::loon::classes::VariableDimensions
    
    variable glyph glyphZoomSensitivity\
	glyphObjects primitiveGlyphTypes

    constructor {args} {
	
	## Configurable option
	set glyph ""
	set glyphZoomSensitivity ""
	
	
	set glyphObjects [::loon::classes::LabelledCollection new\
			      "Glyph" "glyph" "glyph" "%g"]
	
	set primitiveGlyphTypes {circle square diamond triangle\
				     ocircle osquare odiamond otriangle\
				     ccircle csquare cdiamond ctriangle}

	## o for outline
	## c for contrast
	
	next {*}$args
	
	my AddStates "glyph"
	my AddDefaultValue "glyph" "circle"
	my AddStatesToVariableDimension "glyph" "n"
    }
    
    method InfoDebug {args} {
	next glyph glyphObjects {*}$args
    }
    

    method EvalConfigure {} {
	my variable confDict
	
	next
	
	if {[dict get $confDict has_glyph]} {
	    set arg_glyph [dict get $confDict arg_glyph]
	    set n_glyph [llength $arg_glyph]

	    
	    set validGlyphs [concat $primitiveGlyphTypes\
				 [$glyphObjects list]] 
	    
	    foreach g $arg_glyph {
		if {$g ni $validGlyphs} {
		    error "-glyph type \"$g\" is not valid."
		}
	    }
   
	    set new_n [my GetConfigureDim n]
	    if {$n_glyph eq 1} {
		set arg_glyph [lrepeat $new_n $arg_glyph]
	    } elseif {$n_glyph ne $new_n} {
		error "-glyph has length $n_glyph but length 1 or $new_n is expected."
	    }	    
	    
	    
	    dict set confDict new_glyph $arg_glyph
	}

	
	# if init (new n) then remove all glyphs
	if {[dict exists $confDict init_n] && [dict get $confDict init_n]} {
	    foreach glyph [$glyphObjects list] {
		my glyph delete $glyph
	    } 
	}
	

    }

    method ApplyConfigure {} { 
	my variable changedStates
	next
	
	if {"glyph" in $changedStates} {
	    set glyphZoomSensitivity {}
	    foreach g $glyph {
		if {$g in $primitiveGlyphTypes} {
		    lappend glyphZoomSensitivity FALSE
		} else {
		    lappend glyphZoomSensitivity\
			[[$glyphObjects getObject $g] isGlyphZoomSensitive] 
		}
	    }
	}
    }


    method glyph {subcommand args} {
	
	
	switch -- $subcommand {
	    add {
		return [my AddGlyph {*}$args]
	    }
	    delete {
		## delte only one glyph at a time				
		set g $args

		$glyphObjects errorIfIdNotExists $g

		## switch points that have a now\
		## deleted glyph to a new dot
		set hasChanged FALSE
		set newGlyphs $glyph
		set i 0
		foreach gl $newGlyphs {
		    if {$g eq $gl} {
			set hasChanged TRUE
			lset newGlyphs $i circle
		    }
		    incr i
		}
		
		if {$hasChanged} {
		    [self] configure -glyph $newGlyphs
		}

		## Now delete Glyph
		$glyphObjects delete $g
		return
	    }
	    ids {
		$glyphObjects list {*}$args
	    }
	    list {
		puts "use glyph ids instead of glyph list!"
		$glyphObjects $subcommand {*}$args
	    }
	    use -
	    with -
	    target -
	    getObject -
	    getLabel - 
	    getType -
	    relabel {
		$glyphObjects $subcommand {*}$args
	    }
	    type {
		#puts "-type is deprecated"
		$glyphObjects getType {*}$args
	    }
	    default {
		error "\"$subcommand\" is not a valid subcommand for glyph."
	    }
	}
    }
    
    method AddGlyph {type args} {
	
	set cmd [format "%s%s%s" "::loon::classes::"\
		     [string toupper ${type} 0 0] "Glyph"]
	#puts "cmd $cmd"
	
	if {[catch {
	    set obj [uplevel #0 [list $cmd new]]
	} msg]} {
	    error "Glyph type \"$type\" is not knwon or is implemnted incorrectly.\
                   The error message was: $msg"
	}

	my variable n
	[info object namespace $obj]::my SetDimFixed n $n
	
	## get labels if exist
	set label [::loon::listfns::extractAndRemoveOption args -label ""]
	if {[llength $args] > 0} {
	    $obj configure {*}$args
	}

	set id [$glyphObjects nextId]
	
	if {[my isWidget]} {
	    my variable widgetpath
	    set subst [list %W $widgetpath %g $id]
	} else {
	    set subst [list %g $id]
	}
	[info object namespace $obj]::my AddSubstitutions $subst

	# want the add event after setting substitutions
	$glyphObjects add $obj $label $type
	
	return $id
    }
    
    method BindGlyph {systemOrUser cmd args} {
	$glyphObjects bind $systemOrUser $cmd {*}$args
    }

    method SetSubstitutions {subst} {
	$glyphObjects setSubstitutions $subst
	next $subst
    }

    method AddSubstitutions {subst} {
	$glyphObjects addSubstitutions $subst
	next $subst
    }
  

}
