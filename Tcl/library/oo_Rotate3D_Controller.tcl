

::oo::class create ::loon::classes::Rotate3D_Controller {

    superclass ::loon::classes::Canvas_Controller
    
    variable map mouse_x mouse_y rotateMode rotateIndicator rotateIndicatorBg

    constructor {view} {

        set mouse_x 0
        set mouse_y 0
        set rotateMode 0
        set rotateIndicator -1

        set ns [info object namespace $view]
        set map [set [uplevel #0 [list ${ns}::my varname map]]]

        next $view

    }

    method init {} {
        my variable view canvas

        next

        # Always save where the mouse was pressed last
        foreach i {1 2 3} {
            bind $canvas <ButtonPress-$i> "+[self] zp_button %x %y"
        }
        
        bind $canvas <1> "focus %W"
        bind $canvas <KeyPress-r> "+[self] toggleRotateMode"
        bind $canvas <Button1-Motion> "+[self] rotate3DMouse %x %y both; [namespace code {if {$rotateMode eq 1} {break}}]"
        bind $canvas <Control-Button1-Motion> "+[self] rotate3DMouse %x %y y; [namespace code {if {$rotateMode eq 1} {break}}]"
        bind $canvas <Shift-Button1-Motion> "+[self] rotate3DMouse %x %y x; [namespace code {if {$rotateMode eq 1} {break}}]"
        
        bind $canvas <Key-Up> "+[self] rotate3D 0 -1; [namespace code {if {$rotateMode eq 1} {break}}]"
        bind $canvas <Key-Down> "+[self] rotate3D 0 1; [namespace code {if {$rotateMode eq 1} {break}}]"
        bind $canvas <Key-Left> "+[self] rotate3D -1 0; [namespace code {if {$rotateMode eq 1} {break}}]"
        bind $canvas <Key-Right> "+[self] rotate3D 1 0; [namespace code {if {$rotateMode eq 1} {break}}]"
        
    }
    
    method toggleRotateMode {} {
        my variable canvas
        if {$rotateMode eq 0} {
            set rotateMode 1
            if {$rotateIndicator eq -1} {
                set rotateIndicator [$canvas create text 40 30 -text "Rotation Mode" -anchor nw]
                set bbox [$canvas bbox $rotateIndicator]
                set rotateIndicatorBg [$canvas create rectangle $bbox -fill lightgrey -outline lightgrey]
                $canvas raise $rotateIndicator $rotateIndicatorBg
            } else {
                $canvas itemconfigure $rotateIndicator -state normal
                $canvas itemconfigure $rotateIndicatorBg -state normal
            }
        } else {
            set rotateMode 0
            $canvas itemconfigure $rotateIndicator -state hidden
            $canvas itemconfigure $rotateIndicatorBg -state hidden

        }
    }

    method zp_button {x y} {
        set mouse_x $x
        set mouse_y $y
    }
    
    
    method rotate3D {dx dy} {
        my variable model
        
        if {($model eq "") || ($rotateMode eq 0)} {return}
        
        $model configure {*}[$map rotate3DUpdate $dx $dy]
    }

    method rotate3DMouse {xDir yDir direction} {
        my variable model canvas
        
        if {($model eq "") || ($rotateMode eq 0)} {return}
        
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
                my rotate3D $dx $dy
            }
            x {
                my rotate3D $dx 0
            }
            y {
                my rotate3D 0 $dy
            }
        }
        
        set mouse_x $xn
        set mouse_y $yn
        update idletasks
    }

}
