
::oo::class create ::loon::classes::Axes3DVisual {

    superclass ::loon::classes::Visual


    variable xAxis yAxis zAxis xCoords yCoords zCoords xColor yColor zColor axisScaleFactor
    
    constructor {args} {
	
        foreach var {xAxis yAxis zAxis} {
            set $var ""
        }
        set xColor red
        set yColor blue
        set zColor green
        
        set axisScaleFactor 1.0
        
        set xCoords [list 0 0 0]
        set yCoords [list 0 0 0]
        set zCoords [list 0 0 0]
        
        next {*}$args
    }

    method InfoDebug {args} {
        next xAxis yAxis zAxis xCoords yCoords zCoords xColor yColor zColor axisScaleFactor {*}$args
    }
    
    method setAxesCoords {xAx yAx zAx} {
        set xCoords $xAx
        set yCoords $yAx
        set zCoords $zAx
    }

    method setAxisScaleFactor {factor} {
        set axisScaleFactor $factor
    }
    
    method redraw {} {
        my variable canvas visualid isVisible
        
        my clear
        
        if {$isVisible} {set state normal} else {set state hidden}
        
        foreach ax {xAxis yAxis zAxis} color {xColor yColor zColor} {
            set $ax [uplevel #0 [list $canvas create line 0 0 0 0\
                       -tags [list loon $visualid]\
                       -state $state -fill [set $color]]]
        }

        my updateCoords
    }
    
    method updateCoords {} {
        my variable map canvas
        
        if {$xAxis eq ""} {my redraw}

        set bbox [$map getViewport]
        set vpx0 [lindex $bbox 0]
        set vpy0 [lindex $bbox 1]
        set vpx1 [lindex $bbox 2]
        set vpy1 [lindex $bbox 3]
        set xLen [expr {$vpx1 - $vpx0}]
        set yLen [expr {$vpy1 - $vpy0}]
        
        set axesOriginX [expr {$vpx0 + 0.5 * $xLen}]
        set axesOriginY [expr {$vpy1 - 0.5 * $yLen}]
        
        set swap [$map getSwap]
        
        foreach ax {xAxis yAxis zAxis} coord {xCoords yCoords zCoords} color {xColor yColor zColor} {
            if {$swap} {
                set axisEnd [list [expr {$axesOriginX + [lindex [set $coord] 1] * 0.08 * $xLen * $axisScaleFactor}] \
                                  [expr {$axesOriginY - [lindex [set $coord] 0] * 0.08 * $yLen * $axisScaleFactor}]]
            } else {
                set axisEnd [list [expr {$axesOriginX + [lindex [set $coord] 0] * 0.08 * $xLen * $axisScaleFactor}] \
                                  [expr {$axesOriginY - [lindex [set $coord] 1] * 0.08 * $yLen * $axisScaleFactor}]]
            }
            uplevel #0 [list $canvas coords [set $ax]\
                    $axesOriginX $axesOriginY [lindex $axisEnd 0] [lindex $axisEnd 1]]
            
            # Darken axes going into the monitor, brighten those coming out of it
            set brightness [expr {int(100 + 80 * [lindex [set $coord] 2])}]
            uplevel #0 [list $canvas itemconfigure [set $ax] -fill [::tk::Darken [set $color] $brightness]]
        
        }
    }
    
    method clear {} {
        next
        foreach var {xAxis yAxis zAxis} {
            set $var ""
        }
    }

}
