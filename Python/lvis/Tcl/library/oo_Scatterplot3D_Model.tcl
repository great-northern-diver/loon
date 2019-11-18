::oo::class create ::loon::classes::Scatterplot3D_Model {
    
    superclass ::loon::classes::XYPair_Model\
	::loon::classes::withLayers\
	::loon::classes::withGlyphs
    
    variable originalX originalY originalZ

    constructor {args} {
        set originalX ""
        set originalY ""
        set originalZ ""
        
        
        next {*}$args
        
        my New_state z double n ""
        my New_state zlabel string 1 ""
        my New_state dimensionNames string 3 {"" "" ""}
        my New_state rotate3DX double 1 0
        my New_state rotate3DY double 1 0
        my New_state rotationOriginZ double 1 0.0
        my New_state axesCoords nested_double 3 { {1.0 0.0 0.0} {0.0 1.0 0.0} {0.0 0.0 1.0} }
        my New_state axisScaleFactor double 1 1
        
        my SetStateDescription z "z coordinates"
        my SetStateDescription zlabel "z axis label"
        my SetStateDescription dimensionNames \
            "Internal state used to store the dimension names, as x- and ylabel will be updated on rotation"
        my SetStateDescription rotate3DX \
            "Incremental rotation around x axis. (Doesn't store total rotation as chained rotation and panning would get complex)"
        my SetStateDescription rotate3DY \
            "Incremental rotation around y axis. (Doesn't store total rotation as chained rotation and panning would get complex)"
        my SetStateDescription rotationOriginZ \
            "depth component of rotation origin"
        my SetStateDescription axesCoords \
            "Stores the projections of the original x-, y- and z-axes to allow drawing the axis visual"
        my SetStateDescription axisScaleFactor \
            "Factor by which to scale the axis indicators"

        my AddLayer model "scatterplot"\
            [::loon::classes::ScatterplotLayer new [self]] root 0 "Scatterplot"
        
    }
        
    method InfoDebug {args} {
        next {*}$args
        
    }
    
    method getAxesCoords {} {
        my variable axesCoords
        return $axesCoords
    }
    
    method getAxisScaleFactor {} {
        my variable axisScaleFactor
        return $axisScaleFactor
    }
    
    method EvalConfigure {} {
        my variable x y z xTemp yTemp confDict rotate3DX rotate3DY minX maxX minY maxY \
                    panX panY zoomX zoomY deltaX deltaY dimensionNames xlabel ylabel zlabel rotationOriginZ
        
        if {$originalX == ""} {
            set originalX $x
            set dimensionNames [list $xlabel $ylabel $zlabel]
        }
        if {$originalY == ""} {
            set originalY $y
        }
        if {$originalZ == ""} {
            set originalZ $z            
            if {$z ne ""} { set rotationOriginZ [expr {[::loon::listfns::median $z]}] }
        }
        if {([dict exists $confDict has_rotate3DX] && [dict get $confDict has_rotate3DX]) \
         || ([dict exists $confDict has_rotate3DY] && [dict get $confDict has_rotate3DY])} {

            # Set rotation origin to center of display
            set rotationOriginX [expr {$panX + 0.5*$deltaX/$zoomX}]
            set rotationOriginY [expr {$panY + 0.5*$deltaY/$zoomY}]
            
            if {$xTemp != ""} { set x $xTemp }
            if {$yTemp != ""} { set y $yTemp }
            set projected [my project $x $y $z [list $rotationOriginX $rotationOriginY $rotationOriginZ]]
           
            set x [dict get $projected x]
            set y [dict get $projected y]
            set z [dict get $projected z]
            
            dict set confDict new_x $x
            dict set confDict new_y $y
            dict set confDict new_z $z
            
            set mm [::loon::listfns::MinMax $x]
            set minX [lindex $mm 0]
            set maxX [lindex $mm 1]
            set mm [::loon::listfns::MinMax $y]
            set minY [lindex $mm 0]
            set maxY [lindex $mm 1]
            
            my setAxesCoordsAndLabels
        }
        
        next
    }
    
    method setAxesCoordsAndLabels {} {
        my variable axesCoords dimensionNames xlabel ylabel confDict
        # Update coordinates to be fetched by axes3d visual
        set xAxCoords [lindex $axesCoords 0]
        set yAxCoords [lindex $axesCoords 1]
        set zAxCoords [lindex $axesCoords 2]
        set axesCoords [dict values [my project $xAxCoords $yAxCoords $zAxCoords [list 0 0 0]]]
        
        # Set new axis labels
        if {[lindex $xAxCoords 1] >= 0} {set xSign1 "+"} else {set xSign1 "-"}
        if {[lindex $xAxCoords 2] >= 0} {set xSign2 "+"} else {set xSign2 "-"}
        if {[lindex $yAxCoords 1] >= 0} {set ySign1 "+"} else {set ySign1 "-"}
        if {[lindex $yAxCoords 2] >= 0} {set ySign2 "+"} else {set ySign2 "-"}
        dict set confDict new_xlabel [format "%.3f %s %s %.3f %s %s %.3f %s"\
            [lindex $xAxCoords 0] [lindex $dimensionNames 0] $xSign1 \
            [expr {abs([lindex $xAxCoords 1])}] [lindex $dimensionNames 1] $xSign2 \
            [expr {abs([lindex $xAxCoords 2])}] [lindex $dimensionNames 2]]
        dict set confDict new_ylabel [format "%.3f %s %s %.3f %s %s %.3f %s"\
            [lindex $yAxCoords 0] [lindex $dimensionNames 0] $ySign1 \
            [expr {abs([lindex $yAxCoords 1])}] [lindex $dimensionNames 1] $ySign2 \
            [expr {abs([lindex $yAxCoords 2])}] [lindex $dimensionNames 2]]
    }
    
    # Handle reset differently from normal plots:
    # if points are temporarily moved: reset to non-moved state
    # else: reset to non-rotated original state.
    method move {how {which "selected"} args} {
        my variable x y z xTemp yTemp axesCoords dimensionNames n
        
        ## indices
        set sel [my ProcessWhich $which n]
        set n_sel [llength $sel]
        
        switch -- $how {
            resetRotation {
                if {$n_sel eq 0 || $n_sel eq $n} {
                    # Full reset of all points. Also reset the axis indicator.
                    set newx $originalX
                    set newy $originalY
                    set newz $originalZ
                    set newxTemp {}
                    set newyTemp {}
                    
                    set newAxesCoords { {1.0 0.0 0.0} {0.0 1.0 0.0} {0.0 0.0 1.0} }
                    set newxlabel [lindex $dimensionNames 0]
                    set newylabel [lindex $dimensionNames 1]
                    my configure -x $newx -y $newy -z $newz -xTemp $newxTemp -yTemp $newyTemp \
                        -axesCoords $newAxesCoords -xlabel $newxlabel  -ylabel $newylabel
                } else {
                    # Only reset selected points' rotation
                    set newx $x
                    set newy $y
                    set newz $z
                    set newxTemp $xTemp
                    set newyTemp $yTemp
                    foreach ind $sel {
                        lset newx $ind [lindex $originalX $ind]
                        lset newy $ind [lindex $originalY $ind]
                        lset newz $ind [lindex $originalZ $ind]
                        
                        if {[llength $xTemp] > 0} {
                            lset newxTemp $ind [lindex $originalX $ind]
                        }
                        if {[llength $yTemp] > 0} {
                            lset newyTemp $ind [lindex $originalY $ind]
                        }
                    }  
                    my configure -x $newx -y $newy -z $newz -xTemp $newxTemp -yTemp $newyTemp
                }
            }
            default {
                next $how $which {*}$args
            }
        }
    }
    
    method project {xs ys zs rotationCenter} {
        my variable rotate3DX rotate3DY
        
        set cosAX [expr {cos($rotate3DX)}]
        set cosAY [expr {cos($rotate3DY)}]
        set sinAX [expr {sin($rotate3DX)}]
        set sinAY [expr {sin($rotate3DY)}]
        
        # Rotation Matrices
        set Rx [list [list 1.0 0.0 0.0] [list 0.0 $cosAX [expr {-1 * $sinAX}]] [list 0.0 $sinAX $cosAX]]
        set Ry [list [list $cosAY 0.0 $sinAY] [list 0.0 1.0 0.0] [list [expr {-1 * $sinAY}] 0.0 $cosAY]]
        
        set R [::loon::listfns::matmul $Rx $Ry]
        
        set xProjected {}
        set yProjected {}
        set zProjected {}
        foreach xe $xs ye $ys ze $zs {
            # Transform to coordinates relative to rotation origin
            set xShifted [expr {$xe - [lindex $rotationCenter 0]}]
            set yShifted [expr {$ye - [lindex $rotationCenter 1]}]
            set zShifted [expr {$ze - [lindex $rotationCenter 2]}]
            
            # Rotate
            set xTemp [::loon::listfns::dot [lindex $R 0] [list $xShifted $yShifted $zShifted]]
            set yTemp [::loon::listfns::dot [lindex $R 1] [list $xShifted $yShifted $zShifted]]
            set zTemp [::loon::listfns::dot [lindex $R 2] [list $xShifted $yShifted $zShifted]]

            # Transform back to original coordinates
            lappend xProjected [expr {$xTemp + [lindex $rotationCenter 0]}]
            lappend yProjected [expr {$yTemp + [lindex $rotationCenter 1]}]
            lappend zProjected [expr {$zTemp + [lindex $rotationCenter 2]}]
        }
        return [dict create x $xProjected y $yProjected z $zProjected]
    }
}
