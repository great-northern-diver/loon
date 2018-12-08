
oo::class create loon::classes::Plot3DInspectorAnalysis {
    
    superclass ::loon::classes::PlotInspectorAnalysis
    
    constructor {path} {
        next $path
        
    }
    
    method Make {} {
        my variable path
        
        next
        
        # Slider to set rotation center depth
        set depth [frame ${path}.plot.show.depth]
        message ${depth}.depthlabel -text "rotation center\ndepth:" -padx 1
        scale ${depth}.depthslider -from -1 -to 1 -orien horizontal\
                -variable ${depth}.depthVar -resolution -1 -length 170\
                -command [list [self namespace]::my setRotationDepth]
        grid ${depth} -row 6 -column 0 -sticky w -columnspan 6
        grid ${depth}.depthlabel -row 6 -column 0 -sticky w
        grid ${depth}.depthslider -row 6 -column 1 -sticky we -columnspan 5
        
        # Rotation reset button
        set modf ${path}.modify.frame
        set imgResetRotation [image create bitmap\
                  -file [file join $::env(LOON_LIBRARY)\
                         images resetRotation.xbm]]
        
        # Add rotation reset button to normal analysis inspector
        pack [button ${modf}.move.resetRotation\
             -image $imgResetRotation\
             -command [list [self namespace]::my movePoints resetRotation]]\
            -side left
	
	}
    
    method setRotationDepth {depth} {
        my variable plotWidget
        if {$plotWidget ne ""} {
            uplevel #0 [list $plotWidget configure -rotationOriginZ $depth]
        }
    }
    
    method ActivewidgetEvents {args} {
        my variable path plotWidget
        
        if {[llength $args] eq 1} {
            set args {*}$args
        }

        if {"rotationOriginZ" in $args} {
            uplevel #0 [list set ${path}.plot.show.depth.depthVar\
                     [$plotWidget cget -rotationOriginZ]]
        }
        
        # Update slider min and max so one can always set the rotation center depth
        # anywhere within twice the current z range
        if {"rotate3DX" in $args || "rotate3DY" in $args} {
            set minZ [expr {2* [tcl::mathfunc::min {*}[$plotWidget cget -z]]}]
            set maxZ [expr {2* [tcl::mathfunc::max {*}[$plotWidget cget -z]]}]
            ${path}.plot.show.depth.depthslider configure -from $minZ -to $maxZ
        }
        
        next {*}$args
    }
    
    method HookAfterStatesSet {} {
        my variable path plotWidget
        next

        # Set initial rotation center depth
        if {$plotWidget ne ""} {
            uplevel #0 [list set ${path}.plot.show.depth.depthVar\
                         [$plotWidget cget -rotationOriginZ]]
        }
    }
    
}
