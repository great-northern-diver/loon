
## abstract class to use by particular glyph type inspectors
oo::class create loon::classes::GlyphInspector {
    
    superclass ::loon::classes::Inspector2

    ## like Inspector2 but additionaly to activewidget we deal with
    ## activeglyph (namespace)
    
    variable activeglyph glyphBinding

    constructor {Path} {
	
	set activeglyph ""
	set glyphBinding ""
	
	## need no change update
	next $Path ""	
	
	my AddStates activeglyph
    }
    

    method Make {} {
	my variable path
	
	frame $path -class GlyphInspector
	
	pack [label ${path}.label -text "Empty Inspector"] -side left

    }
    
    method CheckNewActivewidget {widget} {
	if {![info object isa typeof $widget\
		  "::loon::classes::withGlyphs"]} {
	    error "$widget does not support glyphs."
	}
    }
    
    method EvalConfigure {} {
	my variable confDict activewidget
	
	next 
	
	if {[dict exists $confDict new_activewidget] &&\
		![dict exists $confDict arg_activeglyph]} {
	    dict set confDict new_glyph ""
	}
	
	if {[dict get $confDict has_activeglyph]} {
	    set arg_glyph [dict get $confDict arg_activeglyph]
	    if {$arg_glyph ne ""} {

		if {[dict exists $confDict new_activewidget]} {
		    set aw [dict get $confDict new_activewidget]
		} else {
		    set aw $activewidget
		}
		if {$aw ne ""} {
		    my CheckNewActiveglyph $aw $arg_glyph
		}
	    }
	    dict set confDict new_activeglyph $arg_glyph
	}
    }
    
    method CheckNewActiveglyph {widget glyph} {
	#puts "Implement CheckNewActiveglyph method"
    }
    
    method ActiveglyphEvents {events} {
	#puts "Implement ActiveglyphEvents method"
    }
    
    
    method UnregisterActivewidget {} {
	my UnregisterActiveglyph
	next
    }
    method UnregisterActiveglyph {} {
	my variable activewidget
	if {$glyphBinding ne ""} {
	    catch {uplevel #0 [list $activewidget glyph use $activeglyph\
				   systembind state delete $glyphBinding]}
	    set glyphBinding ""
	}
    }

    method RegisterActiveglyph {} {
	my variable activewidget

	if {$activewidget ne "" && $activeglyph ne ""} {
	    set glyphBinding\
		[uplevel #0 [list $activewidget glyph use $activeglyph\
				 systembind state add all\
				 "[self namespace]::my ActiveglyphEvents %e"]]
	    my ActiveglyphEvents all
	}
    }
    
    destructor {
	my UnregisterActiveglyph
	next
    }
    
    method ApplyConfigure {} {
	my variable confDict activewidget
	if {[dict exists $confDict new_activeglyph]} {
	    if {$activewidget ne ""} {
		my UnregisterActiveglyph
	    }
	}
	next
    }
    
    method HookAfterStatesSet {} {
	my variable changedStates activewidget
	if {"activeglyph" in $changedStates} {
	    if {$activewidget ne "" && $activeglyph ne ""} {
		my RegisterActiveglyph
	    }
	}
	next
    }

}
