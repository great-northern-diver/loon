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
        my New_state rotate3DX double 1 0
        my New_state rotate3DY double 1 0
        my New_state rotationOrigin double 3 {0.0 0.0 0.0}
        my New_state axesCoords nested_double 3 { {1.0 0.0 0.0} {0.0 1.0 0.0} {0.0 0.0 1.0} }

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
    
    method EvalConfigure {} {
        my variable x y z xTemp yTemp confDict rotate3DX rotate3DY rotationOrigin \
                    panX panY zoomX zoomY deltaX deltaY
        
        if {$originalX == ""} {
            set originalX $x
        }
        if {$originalY == ""} {
            set originalY $y
        }
        if {$originalZ == ""} {
            set originalZ $z
        }
        
        if {([dict exists $confDict has_rotate3DX] && [dict get $confDict has_rotate3DX]) \
         || ([dict exists $confDict has_rotate3DY] && [dict get $confDict has_rotate3DY])} {

            # Set rotation origin to center of display
            set rotationOriginX [expr {$panX + 0.5*$deltaX/$zoomX}]
            set rotationOriginY [expr {$panY + 0.5*$deltaY/$zoomY}]
            
            if {$xTemp != ""} { set x $xTemp }
            if {$yTemp != ""} { set y $yTemp }
            set projected [my project $x $y $z [list $rotationOriginX $rotationOriginY 0]]
           
            set x [dict get $projected x]
            set y [dict get $projected y]
            set z [dict get $projected z]
            
            dict set confDict new_x $x
            dict set confDict new_y $y
            dict set confDict new_z $z

            my setAxesCoordsAndLabels
        }
        
        next
    }
    
    method setAxesCoordsAndLabels {} {
        my variable axesCoords
        # Update coordinates to be fetched by axes3d visual
        set xAxCoords [lindex $axesCoords 0]
        set yAxCoords [lindex $axesCoords 1]
        set zAxCoords [lindex $axesCoords 2]
        set axesCoords [dict values [my project $xAxCoords $yAxCoords $zAxCoords [list 0 0 0]]]
        
        # Set new axis labels
        #set xLabel [format "%s %.3f + %s %.3f"\
		#		    [lindex $curYvars 0] [lindex $xAxCoords 0]\
		#		   [lindex $curYvars 1] [lindex $xAxCoords 1]]
        #set yLabel "[lindex $yAxCoords 0] ${xlabel} + [lindex $yAxCoords 1] ${ylabel} + [lindex $yAxCoords 2] ${zlabel}"
    }
    
    # Handle reset differently from normal plots:
    # if points are temporarily moved: reset to non-moved state
    # else: reset to non-rotated original state.
    method move {how {which "selected"} args} {
        my variable x y z xTemp yTemp axesCoords n
        
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
                    set newAxesCoords $axesCoords
                }
                my configure -x $newx -y $newy -z $newz -xTemp $newxTemp -yTemp $newyTemp -axesCoords $newAxesCoords
            }
            default {
                next $how $which {*}$args
            }
        }
    }
    
    method project {x y z rotationCenter} {
        my variable rotate3DX rotate3DY
        
        set cosAX [expr {cos($rotate3DX)}]
        set cosAY [expr {cos($rotate3DY)}]
        set sinAX [expr {sin($rotate3DX)}]
        set sinAY [expr {sin($rotate3DY)}]
        
        # Rotation Matrices
        set Rx [list [list 1.0 0.0 0.0] [list 0.0 $cosAX [expr {-1 * $sinAX}]] [list 0.0 $sinAX $cosAX]]
        set Ry [list [list $cosAY 0.0 $sinAY] [list 0.0 1.0 0.0] [list [expr {-1 * $sinAY}] 0.0 $cosAY]]
        
        set R [::loon::listfns::matmul $Ry $Rx]
        
        puts stdout "rotateX ${rotate3DX} rotateY ${rotate3DY} R ${R} origin $rotationCenter"
        set xProjected {}
        set yProjected {}
        set zProjected {}
        foreach xe $x ye $y ze $z {
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
