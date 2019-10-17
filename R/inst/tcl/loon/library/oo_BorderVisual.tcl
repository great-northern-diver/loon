
::oo::class create ::loon::classes::BorderVisual {

    superclass ::loon::classes::Visual


    variable idBorderW idBorderE idBorderN idBorderS idBox background foreground 
    
    constructor {args} {
	
	foreach var {idBorderW idBorderE idBorderN idBorderS idBox} {
	    set $var ""
	}
	
	set background $::loon::Options(background)
	set foreground black
	
	next {*}$args
	
    }
    
    method setColor {Background Foreground} {
	my variable canvas
	
	set background $Background 
	set foreground $Foreground
	
	foreach id {idBorderW idBorderE idBorderN idBorderS} {
	    uplevel #0 [list $canvas itemconfigure [set $id] -fill $background]
	}
	uplevel #0 [list $canvas itemconfigure $idBox -outline $foreground]
	
    }

    method InfoDebug {args} {
	next idBorderW idBorderE idBorderN idBorderS idBox {*}$args
    }
    
    method redraw {} {
	my variable canvas visualid isVisible
	
	my clear
	
	if {$isVisible} {set state normal} else {set state hidden}

	foreach rect {idBorderW idBorderE idBorderS idBorderN} {
	    set $rect [uplevel #0 [list $canvas create rect 0 0 0 0\
				       -tag [list loon $visualid]\
				       -state $state -fill $background\
				       -outline ""]]
	}
	
	set idBox [uplevel #0 [list $canvas create rect 0 0 0 0\
				   -tag [list loon $visualid]\
				   -state $state -fill ""\
				   -outline $foreground -width 1]]
	
	my updateCoords
    }
    
    method updateCoords {} {
	my variable map canvas
	
	if {$idBorderW eq ""} {my redraw}

	set canvas_width [winfo width $canvas]
	set canvas_height [winfo height $canvas]
	
	set bbox [$map getViewport]
	set vpx0 [lindex $bbox 0]
	set vpy0 [lindex $bbox 1]
	set vpx1 [lindex $bbox 2]
	set vpy1 [lindex $bbox 3]
	
	# N
	uplevel #0 [list $canvas coords $idBorderN\
			0 0 $canvas_width $vpy0]
	
	# W
	uplevel #0 [list $canvas coords $idBorderW\
			0 0 $vpx0 $canvas_height]

	# S
	uplevel #0 [list $canvas coords $idBorderS\
			0 $canvas_height $canvas_width $vpy1]

	# E
	uplevel #0 [list $canvas coords $idBorderE\
			$vpx1 0 $canvas_width $canvas_height]
	
	# box
	uplevel #0 [list $canvas coords $idBox $vpx0 $vpy0 $vpx1 $vpy1]
	
    }
    
    method clear {} {
	next
	foreach var {idBorderW idBorderE idBorderN idBorderS idBox} {
	    set $var ""
	}
    }

}
