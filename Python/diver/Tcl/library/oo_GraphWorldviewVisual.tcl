::oo::class create ::loon::classes::GraphWorldviewVisual { 
    
    superclass ::loon::classes::GraphVisual

    ## the Scatterplot Worldview Visual is special because
    ## - shows the points always with the same size
    ## - adds a boundingbox around the data
    

    method AddNavigator {navigator} {
	next $navigator TRUE
    } 


    method LayerUpdateDict {events} {
	
	if {[dict exists $events "n"]} {
	    my variable n n_var
	    set n [set $n_var]
	}
	if {[dict exists $events "p"]} {
	    my variable p p_var
	    set p [set $p_var]
	}
	
	if {[dict exists $events "active"]} {
	    my updateActive
	}
	
	if {[dict exists $events "color"]||\
		[dict exists $events "selected"]} {
	    my recolor
	}

	if {[dict exists $events "activeNavigator"]} {
	    my setActiveNavigator
	}
       
    }

    
    
    method clear {} {
	my variable canvas visualid idEdges ids
	
	
	uplevel #0 [list $canvas delete "layer&&$visualid"]
	
	set idEdges {}
	set ids {}

    }
    

    ## need this method because the nodes are always cricles
    method redraw {} {
	my variable canvas visualid isVisible ids tag_var idEdges n_var p_var navigators
	
	my clear
	
	
	set tag [list layer $visualid]
	
	if {[set $n_var] eq 0} {
	    set ids [uplevel #0 [list $canvas create oval 0 0 0 0 -tag $tag -state hidden]]
	} else {
	    ## Edges
	    set idEdges {}
	    for {set i 0} {$i < [set $p_var]} {incr i} {
		lappend idEdges [uplevel #0 [list $canvas create line 0 0 0 0 -tag $tag]]
	    }

	    ## Nodes
	    set ids {}
	    set i 0
	    foreach t [set $tag_var] {
		lappend ids [uplevel #0 [list $canvas create oval 0 0 0 0 -tag $tag]]
		incr i
	    }
	    
	    ## navigators
	    foreach nav [dict keys $navigators] {
		[dict get $navigators $nav visual] redraw   
	    }
	    
	    #my updateTags
	    my updateCoords
	    my recolor
	    my updateActive
	} 
	nextto ::loon::classes::LayerVisual ;## move layer into correct place
	
    }



    method updateCoords {} {
	my variable canvas map isVisible size_var n_var p_var ids curX curY\
	    idEdges iftedges_var
	 
	
	## this is important because the Layer_View will call update coords before
	## state bindings call LayerUpdate method
	if {[llength $ids] ne [set $n_var] || [llength $idEdges] ne [set $p_var]} {
	    my redraw
	}

	
	if {$isVisible && [set $n_var] ne 0} {
	    
	    my updateCurXY
	    	    
	    ## Nodes
	    foreach id $ids\
		x $curX y $curY size [set $size_var] {
		    if {$size < 1} {
		    	set r 1
		    } else {
			set r 2
		    }
		    
		    uplevel #0 [list $canvas coords $id -$r -$r $r $r]
		    uplevel #0 [list $canvas move $id $x $y]
		}
	    
	    ## Edges
	    foreach {ifrom ito} [set $iftedges_var] id $idEdges {
		uplevel #0 [list $canvas coords $id\
				[lindex $curX $ifrom] [lindex $curY $ifrom]\
				[lindex $curX $ito] [lindex $curY $ito]]
	    }
	    
	}
	
	my updateNavigatorCoords
    }


    method recolor {} {
	my variable isVisible canvas n_var ids color_var selected_var
	
	if {$isVisible && [set $n_var] ne 0} {	    
	    set selColor $::loon::Options(select-color)
	    foreach id $ids color [set $color_var] selected [set $selected_var] {
		if {$selected} {
		    uplevel #0 [list $canvas itemconfigure $id\
				    -fill $selColor -outline $selColor]
		} else {
		    uplevel #0 [list $canvas itemconfigure $id\
				    -fill $color -outline $color]
		}
	    }
	}
    }
    
    method setActiveNavigator {} {
	my variable canvas activeNavigator_var
	
	## highlight navigator
	uplevel #0 [list $canvas itemconfigure "model&&navigator"\
			-outline black -width 1]
	
	if {[llength [set $activeNavigator_var]] ne 0} { 
	    uplevel #0 [list $canvas itemconfigure\
			    "model&&navigator&&[set $activeNavigator_var]"\
			    -outline $::loon::Options(select-color)\
			    -width 2]
	}
	
    }
}
