

::oo::class create ::loon::classes::Rotate3D_Controller {
    
    superclass ::loon::classes::Canvas_Controller
    
    variable map mouse_x mouse_y

    constructor {view} {
    
        set mouse_x 0
        set mouse_y 0
        
        set ns [info object namespace $view]
        set map [set [uplevel #0 ${ns}::my varname map]]
        
        next $view
        
    }

    method init {} {
        my variable view canvas

        next

        # Always save where the mouse was pressed last
        foreach i {1 2 3} {
            bind $canvas <ButtonPress-$i> "+[self] zp_button %x %y"
        }
        
        bind $canvas <Button1-Motion> "+[self] rotate3D %x %y both"
        bind $canvas <Alt-Button1-Motion> "+[self] rotate3D %x %y both"
        bind $canvas <Control-Alt-Button1-Motion> "+[self] rotate3D %x %y y"
        bind $canvas <Shift-Alt-Button1-Motion> "+[self] rotate3D %x %y x"

    }
    
    method zp_button {x y} {
        set mouse_x $x
        set mouse_y $y
    }
    
    method rotate3D {xDir yDir direction} {
        my variable model canvas
        
        if {$model eq ""} {return}
        
        set wx [winfo pointerx $canvas]
        set wy [winfo pointery $canvas]
        set rx [winfo rootx $canvas]
        set ry [winfo rooty $canvas]
        
        set xn [expr {$wx-$rx}]
        set yn [expr {$wy-$ry}]
        
        set dx [expr {$xn - $mouse_x}]
        set dy [expr {$yn - $mouse_y}]
        switch -- $direction {
            both {
            $model configure {*}[$map rotate3DUpdate $dx $dy]
            }
            x {
            $model configure {*}[$map rotate3DUpdate $dx 0]
            }
            y {
            $model configure {*}[$map rotate3DUpdate 0 $dy]
            }
        }

        set mouse_x $xn
        set mouse_y $yn
        update idletasks
    }
    
}
