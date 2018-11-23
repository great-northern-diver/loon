
::oo::class create ::loon::classes::Axes3DVisual {

    superclass ::loon::classes::Visual


    variable xAxis yAxis zAxis xCoords yCoords zCoords xColor yColor zColor
    
    constructor {args} {
	
        foreach var {xAxis yAxis zAxis} {
            set $var ""
        }
        set xColor red
        set yColor blue
        set zColor green
        
        set xCoords [list 0 0 0]
        set yCoords [list 0 0 0]
        set zCoords [list 0 0 0]
        
        next {*}$args
    }

    method InfoDebug {args} {
        next xAxis yAxis zAxis xCoords yCoords zCoords xColor yColor zColor {*}$args
    }
    
    method setAxesCoords {xAx yAx zAx} {
        set xCoords $xAx
        set yCoords $yAx
        set zCoords $zAx
    }
    
    method redraw {} {
        my variable canvas visualid isVisible
        
        my clear
        
        if {$isVisible} {set state normal} else {set state hidden}
        
        set xAxis [uplevel #0 [list $canvas create line 0 0 0 0\
                       -tags [list loon $visualid]\
                       -state $state -fill $xColor]]
        set yAxis [uplevel #0 [list $canvas create line 0 0 0 0\
                       -tags [list loon $visualid]\
                       -state $state -fill $yColor]]
        set zAxis [uplevel #0 [list $canvas create line 0 0 0 0\
                       -tags [list loon $visualid]\
                       -state $state -fill $zColor]]
        
        my updateCoords
    }
    
    method updateCoords {} {
        my variable map canvas
        
        if {$xAxis eq ""} {my redraw}

        set canvas_width [winfo width $canvas]
        set canvas_height [winfo height $canvas]
        
        set bbox [$map getViewport]
        set vpx0 [lindex $bbox 0]
        set vpy0 [lindex $bbox 1]
        set vpx1 [lindex $bbox 2]
        set vpy1 [lindex $bbox 3]
        set xLen [expr {$vpx1 - $vpx0}]
        set yLen [expr {$vpy1 - $vpy0}]
        
        set axesOriginX [expr {$vpx0 + 0.5 * $xLen}]
        set axesOriginY [expr {$vpy1 - 0.5 * $yLen}]
        
        set xAxisEnd [list [expr {$axesOriginX + [lindex $xCoords 0] * 0.08 * $xLen }] \
                           [expr {$axesOriginY - [lindex $xCoords 1] * 0.08 * $yLen }]]
                                                                          
        set yAxisEnd [list [expr {$axesOriginX + [lindex $yCoords 0] * 0.08 * $xLen }] \
                           [expr {$axesOriginY - [lindex $yCoords 1] * 0.08 * $yLen }]]
                                                                          
        set zAxisEnd [list [expr {$axesOriginX + [lindex $zCoords 0] * 0.08 * $xLen }] \
                           [expr {$axesOriginY - [lindex $zCoords 1] * 0.08 * $yLen }]]
        
        
        uplevel #0 [list $canvas coords $xAxis\
                $axesOriginX $axesOriginY [lindex $xAxisEnd 0] [lindex $xAxisEnd 1]]
        
        uplevel #0 [list $canvas coords $yAxis\
                $axesOriginX $axesOriginY [lindex $yAxisEnd 0] [lindex $yAxisEnd 1]]

        uplevel #0 [list $canvas coords $zAxis\
                $axesOriginX $axesOriginY [lindex $zAxisEnd 0] [lindex $zAxisEnd 1]]
                
        # Darken axes going into the monitor, brighten those coming out of it
        set xPercent [expr {int(100 - 80 * [lindex $xCoords 2])}]
        uplevel #0 [list $canvas itemconfigure $xAxis\
                -fill [::tk::Darken $xColor $xPercent]]
        set yPercent [expr {int(100 - 80 * [lindex $yCoords 2])}]
        uplevel #0 [list $canvas itemconfigure $yAxis\
                -fill [::tk::Darken $yColor $yPercent]]
        set zPercent [expr {int(100 - 80 * [lindex $zCoords 2])}]
        uplevel #0 [list $canvas itemconfigure $zAxis\
                -fill [::tk::Darken $zColor $zPercent]]
                
        puts stdout "ax3d: $vpx0 $vpx1 $vpy0 $vpy1 $xLen $yLen $xAxisEnd $yAxisEnd $zAxisEnd $axesOriginX $axesOriginY"
    }
    
    method clear {} {
        next
        foreach var {xAxis yAxis zAxis} {
            set $var ""
        }
    }

}
