::oo::class create ::loon::classes::Scatterplot3D_Model {
    
    superclass ::loon::classes::XYPair_Model\
	::loon::classes::withLayers\
	::loon::classes::withGlyphs
    
    variable originalX originalY originalZ rotationOrigin

    constructor {args} {
    set originalX ""
    set originalY ""
    set originalZ ""
    set rotationOrigin [list 0 0 0]
    
	next {*}$args
    
    my New_state z double n ""
    my New_state rotate3DX double 1 0
	my New_state rotate3DY double 1 0

	my AddLayer model "scatterplot"\
	    [::loon::classes::ScatterplotLayer new [self]] root 0 "Scatterplot"
	
    }
    
    
    method InfoDebug {args} {
	next {*}$args
	
    }
    
    method EvalConfigure {} {
        my variable x y z originalX originalY originalZ confDict rotate3DX rotate3DY
        
        set needOrigin FALSE
        if {$originalX == ""} {
            set originalX $x
            set needOrigin TRUE
        }
        if {$originalY == ""} {
            set originalY $y
            set needOrigin TRUE
        }
        if {$originalZ == ""} {
            set originalZ $z
            set needOrigin TRUE
        }
        if {$needOrigin && [llength $originalX] > 0} {
            set xMean [expr {[tcl::mathop::+ {*}$originalX] / [llength $originalX]}]
            set yMean [expr {[tcl::mathop::+ {*}$originalY] / [llength $originalY]}]
            set zMean [expr {[tcl::mathop::+ {*}$originalZ] / [llength $originalZ]}]
            
            set rotationOrigin [list $xMean $yMean $zMean]
        }
        
        if {([dict exists $confDict has_rotate3DX] && [dict get $confDict has_rotate3DX]) \
         || ([dict exists $confDict has_rotate3DY] && [dict get $confDict has_rotate3DY])} {
            set projected [my project $originalX $originalY $originalZ $rotationOrigin]
            set x [dict get $projected x]
            set y [dict get $projected y]
            set z [dict get $projected z]
            
            dict set confDict new_x $x
            dict set confDict new_y $y
            dict set confDict new_z $z
        }
        
        next
    }
    
    method project {x y z rotationCenter} {
        my variable rotate3DX rotate3DY
        
        set cosAX [expr cos($rotate3DX)]
        set cosAY [expr cos($rotate3DY)]
        set sinAX [expr sin($rotate3DX)]
        set sinAY [expr sin($rotate3DY)]
        
        # Rotation Matrices
        set Rx [list [list 1.0 0.0 0.0] [list 0.0 $cosAX [expr {-1 * $sinAX}]] [list 0.0 $sinAX $cosAX]]
        set Ry [list [list $cosAY 0.0 $sinAY] [list 0.0 1.0 0.0] [list [expr {-1 * $sinAY}] 0.0 $cosAY]]
        
        puts stdout "rotateX ${rotate3DX} rotateY ${rotate3DY} Rx ${Rx} Ry ${Ry}"
        set xProjected {}
        set yProjected {}
        set zProjected {}
        foreach xe $x ye $y ze $z {
            # Transform to coordinates relative to rotation origin
            set xShifted [expr {$xe - [lindex $rotationCenter 0]}]
            set yShifted [expr {$ye - [lindex $rotationCenter 1]}]
            set zShifted [expr {$ze - [lindex $rotationCenter 2]}]
            
            # Rotate
            set xTemp [::loon::listfns::dot [lindex $Ry 0] [list $xShifted $yShifted $zShifted]]
            set yTemp [::loon::listfns::dot [lindex $Ry 1] [list $xShifted $yShifted $zShifted]]
            set zTemp [::loon::listfns::dot [lindex $Ry 2] [list $xShifted $yShifted $zShifted]]
        
            set xTemp [::loon::listfns::dot [lindex $Rx 0] [list $xTemp $yTemp $zTemp]]
            set yTemp [::loon::listfns::dot [lindex $Rx 1] [list $xTemp $yTemp $zTemp]]
            set zTemp [::loon::listfns::dot [lindex $Rx 2] [list $xTemp $yTemp $zTemp]]
            
            # Transform back to original coordinates
            lappend xProjected [expr {$xTemp + [lindex $rotationCenter 0]}]
            lappend yProjected [expr {$yTemp + [lindex $rotationCenter 1]}]
            lappend zProjected [expr {$zTemp + [lindex $rotationCenter 2]}]
        }
        return [dict create x $xProjected y $yProjected z $zProjected]
    }
}
