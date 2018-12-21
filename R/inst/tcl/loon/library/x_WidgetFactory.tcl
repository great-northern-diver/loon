
namespace eval loon {
    variable open_widgets
    set open_widgets {}
}

proc WidgetFactory {type postfix args} {
    
    if {[string index [lindex $args 0] 0] eq "." } {	
 	set path [lindex $args 0]
     	set args [lreplace $args 0 0]
     	set needPacking FALSE
    } else {
     	set needPacking TRUE
    	
	## If toplevel already exists
	#while {[winfo exists .l${::loon::toplevelCounter}]} {
     	#    incr ::loon::toplevelCounter
     	#}
	
     	#set top .l${::loon::toplevelCounter}
	#toplevel $top
	
	
	set top [::loon::loon_toplevel]
	wm iconphoto $top $::env(LOON_ICON)	
	set path ${top}.${postfix}
	
	wm title $top "$type with widget path $path"
    }

    
    ## Standard Tk behavior is to have the widget path name as the
    ## accessor/modifier command of the widget model. In loon we split
    ## up view and model into two different objects. The user should
    ## not notice the view/model distinction as the widget path name
    ## is used to pack a widget and to access/modify it too. Hence,
    ##
    ## for the widget path: .loonwin1.plot
    ##
    ##
    ## create model object without a specific command location put
    ## tell it the widget path
    ##
    ## create view object, a Tk widget, at: .loonwin1.plot
    ##
    ## rename the accessor command for the widget at .loonwin1.plot to
    ## .loonwin1.plot.fr (assuming it is a frame)
    ##
    ## rename model object command to widget path

    set isPlotDisplay FALSE
    switch -- $type {
	Scatterplot -
    Scatterplot3D -
	Graph -
	Serialaxes -
	Histogram {
	    set widget [::loon::classes::${type}_Widget new $path]
	    set isPlotDisplay TRUE
	}
	default {
	    ## Inspector2 Widgets
	    ## Widgets that inherit from Inspector2
	    set o [::loon::classes::$type new $path]
	    uplevel #0 rename $path $path.fr
	    uplevel #0 rename $o $path

	    ## To delete as soon all widgets work
	    #set model [loon::classes::${type}_Model new $path]    
	    #set view [loon::classes::${type}_View new $path]
	    
	    #uplevel #0 rename $path $path.fr
	    #uplevel #0 rename $model $path
	    
	    #$view setModel $path
	}
    }
    
    if {[llength $args] > 0} {
	$path configure {*}$args
	if {$isPlotDisplay && $type ne "Serialaxes" && [$path cget -n]>0} {
	    set toAppend ""

	    if {"-deltaX" ni $args && "-x" in $args} {
		
		set deltaX [::loon::listfns::drange [$path cget -x]]
		if {$deltaX eq 0 || $deltaX eq 0.0} {
		    set deltaX 0.0005
		}
		set toAppend [list -deltaX $deltaX]
	    }
	    if {"-deltaY" ni $args && "-y" in $args} {
		set deltaY [::loon::listfns::drange [$path cget -y]]
		if {$deltaY eq 0 || $deltaY eq 0.0} {
		    set deltaY 0.0005
		}
		lappend toAppend -deltaY $deltaY
	    }
	    #puts "drange: $toAppend"
	    if {[llength $toAppend]>0} {
		$path configure {*}$toAppend
	    }	    
	}
    }
    
    if {$needPacking} {
	update
	pack $path -fill both -expand 1
	##pack [::ttk::sizegrip ${top}.sizegrip] -side right -anchor se
	
	## also destroy toplevel
	$path systembind state add destroy "destroy $top"
    }
    
    
    if {$isPlotDisplay} {
	::loon::setLoonInspectorActiveWidget $path
	
	## <ButtonPress> alone will not be evaluated if <ButtonPress-1>
	## is defined
	foreach i {1 2 3} {
	    bind ${path}.canvas <ButtonPress-$i>\
		"+::loon::setLoonInspectorActiveWidget $path"
	}

	if {$needPacking} {
	    bind $top <FocusIn>\
		"+::loon::setLoonInspectorActiveWidget $path"
	}
	
    }
    
    lappend ::loon::open_widgets $path
    bind $path <Destroy> "+::loon::removeKnownWidget $path"
    
    ## view destroys model
    bind $path <Destroy> "+$path destroy"
    
    update idletasks
    if {$isPlotDisplay && $type ne "Serialaxes"} {
	$path scaleto world
    }
    return $path 

}

proc ::loon::isKnownWidget {widget} {
    variable open_widgets
    if {$widget in $open_widgets} {
	return TRUE
    } else {
	return FALSE
    }
}
proc ::loon::removeKnownWidget {widget} {
    variable open_widgets
    set i [lsearch -exact $open_widgets $widget]
    if {$i ne "-1"} {
	set open_widgets [lreplace $open_widgets $i $i]
    }
}
