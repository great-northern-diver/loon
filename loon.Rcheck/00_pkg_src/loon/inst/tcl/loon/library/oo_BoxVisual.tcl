
oo::class create loon::classes::BoxVisual {
    
    superclass ::loon::classes::Visual

    variable id x0 y0 x1 y1 color linecolor linewidth dash
    
    constructor {Color Linecolor Linewidth Dash args} {
	
	set id ""
	set x0 0
	set x1 1
	set y0 0
	set y1 1

	set color $Color
	set linecolor $Linecolor
	set linewidth $Linewidth
	set dash $Dash
	
	next {*}$args
    }
    
    
    method clear {} {
	next 
	set id ""
    }
    
    method setCoords {X0 Y0 X1 Y1} {
	set x0 $X0
	set y0 $Y0
	set x1 $X1
	set y1 $Y1
	my updateCoords
    }
    
    #method setX0 {X0} {	set x0 $X0 }
    #method setX1 {X1} { set x1 $X1 }
    #method setY0 {Y0} { set y0 $Y0 }
    #method setY1 {Y1} { set y1 $Y1 }
   
    
    method redraw {} {
	my variable map canvas visualid isVisible
	
	if {$id ne ""} {
	    my clear
	}
	
	
	if {$isVisible} {set state normal} else {set state hidden}
	
	set id [uplevel #0 [list $canvas create rect 0 0 0 0\
				-fill $color\
				-width $linewidth\
				-outline $linecolor\
				-dash $dash\
				-tag $visualid\
				-state $state]]
	my updateCoords
	
    }
    
    method updateCoords {} {
	my variable map canvas isVisible	
	
	if {$isVisible} {
	    if {$id eq ""} {my redraw}
	    
	    uplevel #0 [list $canvas coords $id\
			    [$map mapDxy2Scoords [list $x0 $x1] [list $y0 $y1]]]
	}
	
    }
    
    method info {} {
	next x0 y0 x1 y1 color linecolor linewidth dash {*}$args
    }
    
    
}
