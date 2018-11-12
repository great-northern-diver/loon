::oo::class create ::loon::classes::Scatterplot3D_Model {
    
    superclass ::loon::classes::XYPair_Model\
	::loon::classes::withLayers\
	::loon::classes::withGlyphs
    
    variable pi originalX originalY originalZ angleX angleY

    constructor {args} {
    set pi 3.1415926535897931
    set originalX ""
    set originalY ""
    set originalZ ""
    # angleX is 0 if the projection plane is not rotated along the x axis
    set angleX 0
    set angleY 0
    
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
        if {$originalX == ""} {
            set originalX $x
        }
        if {$originalY == ""} {
            set originalY $y
        }
        if {$originalZ == ""} {
            set originalZ $z
        }
        
        set needProject FALSE
        if {[dict exists $confDict has_rotate3DX]} {
            set angleX [expr {($rotate3DX * $pi / (360))}]
            set needProject TRUE
        }
        if {[dict exists $confDict has_rotate3DY]} {
            set angleY [expr {($rotate3DY * $pi / (360))}]
            set needProject TRUE
        }
        if {$needProject} { my project }
        
        next
    }
    
    method project {} {
        my variable x y z originalX originalY originalZ angleX angleY
        
        set cosAX [expr cos($angleX)]
        set cosAY [expr cos($angleY)]
        set sinAX [expr sin($angleX)]
        set sinAY [expr sin($angleY)]
        
        # Rotation Matrices
        set Rx [list [list 1.0 0.0 0.0] [list 0.0 $cosAX [expr {-1 * $sinAX}]] [list 0.0 $sinAX $cosAX]]
        set Ry [list [list $cosAY 0.0 $sinAY] [list 0.0 1.0 0.0] [list [expr {-1 * $sinAY}] 0.0 $cosAY]]
        
        puts stdout "angleX ${angleX} angleY ${angleY} Rx ${Rx} Ry ${Ry}"
        
        set x {}
        set y {}
        set z {}
        foreach xe $originalX ye $originalY ze $originalZ {
            set xTemp [::loon::listfns::dot [lindex $Ry 0] [list $xe $ye $ze]]
            set yTemp [::loon::listfns::dot [lindex $Ry 1] [list $xe $ye $ze]]
            set zTemp [::loon::listfns::dot [lindex $Ry 2] [list $xe $ye $ze]]
        
            lappend x [::loon::listfns::dot [lindex $Rx 0] [list $xTemp $yTemp $zTemp]]
            lappend y [::loon::listfns::dot [lindex $Rx 1] [list $xTemp $yTemp $zTemp]]
            lappend z [::loon::listfns::dot [lindex $Rx 2] [list $xTemp $yTemp $zTemp]]
        }
    }
}
