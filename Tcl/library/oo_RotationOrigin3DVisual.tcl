
::oo::class create ::loon::classes::RotationOrigin3DVisual {

    superclass ::loon::classes::Visual
    
    variable color origin
    
    constructor {args} {
        
        set color blue
        set origin ""
        
        next {*}$args
    }

    method InfoDebug {args} {
        next color {*}$args
    }
    
    method redraw {} {
        my variable canvas visualid isVisible map
        
        my clear
        
        if {$isVisible} {set state normal} else {set state hidden}
        
        set canvas_width [winfo width $canvas]
        set canvas_height [winfo height $canvas]
        
        set bbox [$map getViewport]
        set vpx0 [lindex $bbox 0]
        set vpy0 [lindex $bbox 1]
        set vpx1 [lindex $bbox 2]
        set vpy1 [lindex $bbox 3]
        set xLen [expr {$vpx1 - $vpx0}]
        set yLen [expr {$vpy1 - $vpy0}]
        puts "st $state"
        set origin [uplevel #0 [list $canvas create text [expr {$vpx0 + $xLen / 2}] [expr {$vpy0 + $yLen / 2}] -text "x" \
                   -tags [list loon $visualid]\
                   -state $state -fill $color]]
    }
    
    #method updateCoords {} {
    #    my variable map canvas
    #    
    #    if {$xAxis eq ""} {my redraw}
    #
    #    set canvas_width [winfo width $canvas]
    #    set canvas_height [winfo height $canvas]
    #    
    #    set bbox [$map getViewport]
    #    set vpx0 [lindex $bbox 0]
    #    set vpy0 [lindex $bbox 1]
    #    set vpx1 [lindex $bbox 2]
    #    set vpy1 [lindex $bbox 3]
    #    set xLen [expr {$vpx1 - $vpx0}]
    #    set yLen [expr {$vpy1 - $vpy0}]
    #    
    #    set axesOriginX [expr {$vpx0 + 0.05 * $xLen}]
    #    set axesOriginY [expr {$vpy1 - 0.05 * $yLen}]
    #    
    #    set xAxisEnd [list [expr {$axesOriginX + [lindex $xCoords 0] * 0.05 * $xLen }] \
    #                       [expr {$axesOriginY - [lindex $xCoords 1] * 0.05 * $yLen }]]
    #    
    #    set yAxisEnd [list [expr {$axesOriginX + [lindex $yCoords 0] * 0.05 * $xLen }] \
    #                       [expr {$axesOriginY - [lindex $yCoords 1] * 0.05 * $yLen }]]
    #    
    #    set zAxisEnd [list [expr {$axesOriginX + [lindex $zCoords 0] * 0.05 * $xLen }] \
    #                       [expr {$axesOriginY - [lindex $zCoords 1] * 0.05 * $yLen }]]
    #    
    #    
    #    uplevel #0 [list $canvas coords $xAxis\
    #            $axesOriginX $axesOriginY [lindex $xAxisEnd 0] [lindex $xAxisEnd 1]]
    #    
    #    uplevel #0 [list $canvas coords $yAxis\
    #            $axesOriginX $axesOriginY [lindex $yAxisEnd 0] [lindex $yAxisEnd 1]]
    #
    #    uplevel #0 [list $canvas coords $zAxis\
    #            $axesOriginX $axesOriginY [lindex $zAxisEnd 0] [lindex $zAxisEnd 1]]
    #            
    #    puts stdout "ax3d: $vpx0 $vpx1 $vpy0 $vpy1 $xLen $yLen $xAxisEnd $yAxisEnd $zAxisEnd $axesOriginX $axesOriginY"
    #}
    
    method clear {} {
        next
        set origin ""
    }

}
