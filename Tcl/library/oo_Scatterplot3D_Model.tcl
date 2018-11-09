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
        
        set projectionPlaneNormal [list $sinAY $sinAX [expr {$cosAX + $cosAY}] ]
        
        if {$sinAY == 0 && $sinAX == 0} {
            set e1 {-1 0 0}
            set e2 {0 -1 0}
        } else {
            #ax=0 pi 2pi   => sina=0 cosa=1
            #ax=pi/2 3pi/2 => sina=0 cosa=1
            # e1: -sinax sinay 0
            # e2: -sinay*(cosax+cosay) -sinax*(cosax+cosay) sinay^2 + sinax^2
            set e1 [::loon::listfns::norm [list [expr {-[lindex $projectionPlaneNormal 1]}] [lindex $projectionPlaneNormal 0] 0 ]]
            set e2 [::loon::listfns::norm [list [expr {-[lindex $projectionPlaneNormal 0]*[lindex $projectionPlaneNormal 2]}] \
                        [expr {-[lindex $projectionPlaneNormal 1] * [lindex $projectionPlaneNormal 2]}] \
                        [expr {[lindex $projectionPlaneNormal 0] * [lindex $projectionPlaneNormal 0] + [lindex $projectionPlaneNormal 1] * [lindex $projectionPlaneNormal 1]}]]]
        }
        puts stdout "angleX ${angleX} angleY ${angleY}, plane ${projectionPlaneNormal}, e1 ${e1} e2 ${e2} "
        set xNew {}
        set yNew {}
        set zNew {}
        foreach xe $originalX ye $originalY ze $originalZ {
            lappend xNew [::loon::listfns::dot [list $xe $ye $ze] $e1]
            lappend yNew [::loon::listfns::dot [list $xe $ye $ze] $e2]
            lappend zNew [::loon::listfns::dot [list $xe $ye $ze] [::loon::listfns::norm $projectionPlaneNormal]]
        }
        set x $xNew
        set y $yNew
        set z $zNew
    }
    
     
    

    

}
