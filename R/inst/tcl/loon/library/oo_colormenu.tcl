
## Re-implementation of color-menu using objects


namespace eval loon {
    namespace export colormenu

    proc colormenu {path args} {
	set o [::loon::classes::colormenu new $path]

	if {[llength $args]>0} {
	    $o configure {*}$args
	}
	
	uplevel #0 rename $path $path.fr
	uplevel #0 rename $o $path
	
	return $path
    }
    
}

oo::class create ::loon::classes::colormenu {
    
    superclass ::loon::classes::Configurable
    
    variable path canvas scrollbar max_canvas_width isDrawing
    
    constructor {Path} {
	set path $Path
	set max_canvas_width 1
	set isDrawing FALSE

	my Make $path
	my Bind

	
	next
	
	my New_state color color any\
	    [::loon::listfns::toHexcolor $::loon::Options(colors)]
	my New_state command string 1 ""
	my New_state boxwidth positive_integer 1 12
	my New_state boxborder positive_integer 1 6
	my New_state maxNrows positive_integer 1 3
	my New_state hasAddColor boolean 1 TRUE
	
	# get active and background color
	set b [button ${path}.b]
	my New_state background color 1 [$b cget -background]
	my New_state activebackground color 1 [$b cget -activebackground]
	destroy $b


	my SetStateDescription color\
	    "color list for color boxes"
	my SetStateDescription command\
	    "callback evaluated when pressing on a color box"
	my SetStateDescription boxwidth\
	    "with of a color box"
	my SetStateDescription boxborder\
	    "thickness of a color box"
	my SetStateDescription maxNrows\
	    "max. number of rows displayed before adding a slider on the side"
	my SetStateDescription hasAddColor\
	    "has two add color boxes (+5 and +)"
	my SetStateDescription background\
	    "background color of widget"
	my SetStateDescription activebackground\
	    "background color of a color box when mouse enters"

    }
    
    method Make {path} {
	frame $path -class LoonColormenu
	
	set scrollbar [scrollbar ${path}.vscroll -orient vertical]
	set canvas [canvas ${path}.canvas -width 200 -height 100\
			-yscrollcommand "$scrollbar set"]
	$scrollbar configure -command "$canvas yview"
	
	pack $scrollbar -side right -fill y
	pack $canvas -side left -fill x -expand 1	
    }

    method Canvas_scroll {D} {
	if {($D < 0 && [lindex [$canvas yview] 0] > 0)\
		|| ($D > 0 && [lindex [$canvas yview] 1] < 1)} {
	    $canvas yview scroll $D units
	}
    }
    method Scrollbas_scroll {D} {
	if {$D < 0 && [lindex [$canvas yview] 0] ne 0.0} {
	    $canvas yview scroll $D units
	}
    }
    
    method Add_color {} {
	set new_col [tk_chooseColor\
			 -initialcolor white\
			 -parent [winfo toplevel $path]]
	
	if {$new_col ne ""} {
	    my variable color
	    my configure -color [concat $color $new_col]
	}
    }

    method Add_five {} {
	my variable color
	
	set cols [::loon::hcl::hue_mem_pal [expr {[llength $color] + 5}]\
		      {*}$::loon::Options(colors-palette-hcl)]
	set new_cols [lrange $cols end-4 end]	
	my configure -color [concat $color $new_cols]
    }
    
    method Remove_color {} {
	my variable color
	set i [lindex [$canvas itemcget current -tag] 2]
	if {$i ne "+"} {
	    my configure -color [lreplace $color $i $i]
	}
    }
    
    method Enter_colorbox {} {
	my variable activebackground
	set i [lindex [$canvas itemcget current -tag] 2]
	$canvas itemconfigure "outer && $i" -fill $activebackground
    }
    method Leave_colorbox {} {
	my variable background
	set i [lindex [$canvas itemcget current -tag] 2]
	$canvas itemconfigure "outer && $i" -fill $background
    }
    
    method Command_eval {} {
	my variable command color
	
	set i [lindex [$canvas itemcget current -tag] 2]
	if {$i ne "+"} {
	    set col [lindex $color $i]
	    uplevel #0 [string map [list %c $col] $command]
	}       
    }

    method Bind {} {
	bind $canvas <Configure> "+[self namespace]::my Resize"
	
	if {$::tcl_platform(os) in {"Darwin" "Windows"}} {
	    # zoom windows	    
	    bind $canvas <MouseWheel> "[self namespace]::my Canvas_scroll %D"
	    
	    bind $scrollbar <MouseWheel> "[self namespace]::my Scrollbar_scroll %D"	    
	} else {	   
	    bind $canvas <Button-4> "[self namespace]::my Canvas_scroll -1"
	    bind $canvas <Button-5> "[self namespace]::my Canvas_scroll 1"
	    
	    bind $scrollbar <Button-4> "[self namespace]::my Scrollbar_scroll -1"
	    bind $scrollbar <Button-5> "[self namespace]::my Scrollbar_scroll 1"
	}
	
	$canvas bind "add" <Button-1> "[self namespace]::my Add_color"
	$canvas bind "addFive" <Button-1> "[self namespace]::my Add_five"
	$canvas bind "color || + || addFive" <Enter> "[self namespace]::my Enter_colorbox"
	$canvas bind "color || + || addFive" <Leave> "[self namespace]::my Leave_colorbox"
	
	$canvas bind "color" <Button-1> "[self namespace]::my Command_eval"

	$canvas bind "color" <Button-3> "[self namespace]::my Remove_color"
    }
    
    method redraw {} {
	my variable color maxNrows hasAddColor boxwidth boxborder\
	    background activebackground
	
	set isDrawing TRUE
	
	set nColor [llength $color]
	
	set nBoxes $nColor
	if {$hasAddColor} {incr nBoxes 2}
	
	set colorbox_width [expr {$boxwidth +2*$boxborder}]
	set nBoxesPerRow [expr {max(int($max_canvas_width/double($colorbox_width)),1)}]
	
	set nRows [expr {int(ceil(double($nBoxes)/$nBoxesPerRow))}]
	
	if {$nRows > $maxNrows} {
	    pack forget $canvas
	    pack $scrollbar -side right -fill y
	    pack $canvas -side left -fill both -expand TRUE 
	    set hasScrollbar TRUE
	} else {
	    pack forget $scrollbar
	    set hasScrollbar FALSE
	}
	update idletasks
	
	set width [winfo width $canvas]
	set nBoxesPerRow [expr {max(int($width/double($colorbox_width)),1)}]
	
	if {$hasScrollbar} {
	    $canvas configure -height [expr {$maxNrows*$colorbox_width}]\
		-scrollregion [list 0 0 $width [expr {$nRows*$colorbox_width}]]
	} else {
	    set height [expr {$nRows * $colorbox_width}]
	    $canvas configure -height $height\
		-scrollregion [list 0 0 $width $height]
	}


	## Drawing Part
	$canvas delete all
	
	set so2 [expr {$colorbox_width/2.0}]
	set si2 [expr {$boxwidth/2.0}]
	
	set i 0; set i_x 0; set i_y 0
	foreach col $color {
	    $canvas create rect\
		-$so2 -$so2 $so2 $so2\
		-width 0\
		-fill $background\
		-tag [list color outer $i]
	    $canvas create rect\
		-$si2 -$si2 $si2 $si2\
		-width 1\
		-fill $col\
		-tag [list color inner $i]
	    
	    set dx [expr {$so2 + $i_x*$colorbox_width}]
	    set dy [expr {$so2 + $i_y*$colorbox_width}]
	    
	    $canvas move "color && $i" $dx $dy
	    
	    incr i
	    incr i_x
	    if {$i_x == $nBoxesPerRow} {
		set i_x 0
		incr i_y
	    }	    
	}

	if {$hasAddColor} {
	    ## pick color
	    $canvas create rect\
		-$so2 -$so2 $so2 $so2\
		-width 0\
		-fill $background\
		-tag [list add outer +]
	    
	    $canvas create line\
		0 -$si2 0 $si2\
		-width 1\
		-tag [list add line +]
	    
	    $canvas create line\
		-$si2 0 $si2 0\
		-width 1\
		-tag [list add line +]
	    
	    set dx [expr {$so2 + $i_x*$colorbox_width}]
	    set dy [expr {$so2 + $i_y*$colorbox_width}]
	    
	    $canvas move "add" $dx $dy

	    incr i
	    incr i_x
	    if {$i_x == $nBoxesPerRow} {
		set i_x 0
		incr i_y
	    }	    
	    
	    ## add five
	    $canvas create rect\
		-$so2 -$so2 $so2 $so2\
		-width 0\
		-fill $background\
		-tag [list addFive outer +5]

	    $canvas create text 0 0 -text "+5" -tag [list addFive inner +5]
	    
	    set dx [expr {$so2 + $i_x*$colorbox_width}]
	    set dy [expr {$so2 + $i_y*$colorbox_width}]
	    
	    $canvas move "addFive" $dx $dy
	    
	}
	set isDrawing FALSE
    }


    method Resize {} {
	if {!$isDrawing} {
	    ## How Many Rows are needed?
	    pack forget $scrollbar
	    update idletasks; # to get the correct width
	    
	    set max_canvas_width [winfo width $canvas]
	    my redraw
	}
    }
        
    method ApplyConfigure {} {
	my variable changedStates
	next
	
	if {[llength $changedStates] > 0} {
	    my redraw
	}
	
    }
    
    
}
