
oo::class create loon::classes::ColorCrossTable {

    superclass ::loon::classes::Linkable\
	::loon::classes::Inspector2

    variable canvas groupDict colorDict crossDict selectedDict max hR hB hG lR lG lB dR dG dB

    constructor {Path} {
	
	set groupDict ""
	set colorDict ""
	set crossDict ""
	set selectedDict ""
	
	set hR 255
	set hG 0
	set hB 0
	set lR 255
	set lG 255
	set lB 255
	set dR 0
	set dG -255
	set dB -255

	set max 0

	next $Path
	
	my New_state group string n ""
	my New_state color color n steelblue
	my New_state selected boolean n FALSE
	
	my SetInitStates n group
	my setLinkedStates {color selected}

	my New_state title string 1 "Cross Table"

	my New_state high color 1 purple
	my New_state low color 1 white

	my New_state gridcolor color 1 gray80
	
	my New_state cellwidth positive_integer 1 40
	my New_state cellheight positive_integer 1 30
	
	my New_state showNumber boolean 1 FALSE
	
	my SetStateDescription group\
	    "group used for table columns"
	my SetStateDescription color\
	    "color used for table rows"
	my SetStateDescription selected\
	    "boolean for specifying that a data point is selected or not"
	my SetStateDescription title\
	    "title of plot"
	my SetStateDescription high\
	    "color to indicate that cell is 'full'"
	my SetStateDescription low\
	    "color to indicate that cell is 'empty'"
	my SetStateDescription gridcolor\
	    "color used for drawing grid lines"
	my SetStateDescription cellwidth\
	    "cell width in pixels"
	my SetStateDescription cellheight\
	    "cell height in pixels"
	my SetStateDescription showNumber\
	    "not implemented yet"
	
	my UpdateRGB
	my redraw
    }

    method CheckNewActivewidget {widget} {}
    
    method HookAfterStatesSet {} {
	my variable changedStates group color selected

	set needRecalc FALSE

	if {"group" in $changedStates} {
	    set needRecalc TRUE
	    
	    set groupDict [dict create]
	    
	    foreach g $group {
		dict incr groupDict $g
	    }
	    
	    #set groupnames [dict keys $dtmp]
	    #set ngroups [llength $groupnames]

	    set max 0
	    dict for {name val} $groupDict {
		if {$val > $max} {set max $val}
	    }
	    
	}

	if {"color" in $changedStates} {
	    set needRecalc TRUE
	    set colorDict [dict create]
	    foreach col $color {
		dict incr colorDict $col
	    }
	}
	
	if {$needRecalc} {
	    set crossDict [dict create]
	    
	    foreach col $color g $group {
		dict incr crossDict "${col}-${g}" 
	    }
	    	    
	}
	    
	    

	if {"selected" in $changedStates} {
	    set selectedDict [dict create]
	    
	    foreach sel $selected g $group {
		if {$sel} {
		    dict incr selectedDict $g
		}
	    }
	}
	
	if {"low" in $changedStates || "high" in $changedStates} {
	    my UpdateRGB
	}
	

	next
	my redraw
    }

    
    method Make {} {
	my variable path
    
	frame $path -class LoonCrossTable
	set canvas [canvas ${path}.canvas -bg white -width 600 -heigh 400]

	pack $canvas -fill both -expand TRUE
    }

    method redraw {} {
	my variable title gridcolor cellwidth cellheight linkingGroup low high
	
	$canvas delete all
	
	## Title 
	$canvas create text 20 50 -text $title -font "Arial 18 bold" -anchor sw
	$canvas create text 20 65 -text "linking group: $linkingGroup" -anchor sw
	
	set width [winfo width $canvas]
	set height [winfo height $canvas]
	
	set border 40
	
	## 
	set selectedColor $::loon::Options(select-color)
	
	set cw2 [expr {$cellwidth/2.0}]
	set ch2 [expr {$cellheight/2.0}]
	set x0 130
	set y0 190
	

	set groupnames [dict keys $groupDict]
	set colornames [dict keys $colorDict]
	
	
	## Column Names
	set i 0
	foreach g $groupnames {
	    $canvas create text [expr {$x0 + $cw2 + $i*$cellwidth}] $y0  -text $g\
		-angle 90 -anchor w
	    incr i
	}
	set x1 [expr {$x0 + $i*$cellwidth}]
	

	

	set y0 200
	
	## selected
	set yc0 $y0
	set yc1 [expr {$y0+$cellheight}]
	$canvas create rect 40 $y0 $x0 $yc1 -fill $selectedColor -outline ""
	set j 0
	foreach g $groupnames {
	    set xc0 [expr {$x0 + $j * $cellwidth}]
	    incr j
	    set xc1 [expr {$x0 + $j * $cellwidth}]
	    
	    if {[dict exists $selectedDict $g]} {
		set count [dict get $selectedDict $g]
	    } else {
		set count 0
	    }
	    
	    $canvas create rect $xc0 $yc0 $xc1 $yc1\
		-fill [my GetColorFromValue $count [dict get $groupDict $g]]\
		-outline ""
	}


	## Now draw colors
	set i 1
	foreach col $colornames {
	    set yc0 [expr {$y0 + $i*$cellheight}]
	    incr i
	    set yc1 [expr {$y0 + $i*$cellheight}]
	    
	    
	    $canvas create rect 40 $yc0 $x0 $yc1 -fill $col -outline ""
	    
	    set j 0
	    foreach g $groupnames {
		
		set xc0 [expr {$x0 + $j*$cellwidth}]
		incr j
		set xc1 [expr {$x0 + $j*$cellwidth}]

		set key "${col}-${g}"
		if {[dict exists $crossDict $key]} {
		    set count [dict get $crossDict $key]
		} else {
		    set count 0
		}
		
		$canvas create rect $xc0 $yc0 $xc1 $yc1\
		    -fill  [my GetColorFromValue $count [dict get $groupDict $g]]\
		    -outline ""
			
	    }
	    
	}

	set y1 [expr {$y0 + $i*$cellheight}]
	



	
	set y0 200
	set ncolors [llength $colornames]
	for {set i 0} {$i <= [expr {$ncolors+1}]} {incr i} {
	    set y [expr {$y0 + $i*$cellheight}]
	    $canvas create line 40 $y $x1 $y -fill $gridcolor
	}

	

	## Grids
	set i 0
	foreach g $groupnames {
	    set x [expr {$x0 + $i*$cellwidth}]
	    $canvas create line $x 80 $x $y1  -fill $gridcolor
	    incr i
	}
	set x [expr {$x0 + $i*$cellwidth}]
	$canvas create line $x 80 $x $y1 -fill $gridcolor

	
	## Legend
	set xl0 [expr {$x1 + 50}]
	set xl1 [expr {$xl0 + $cellwidth}]
	
	set yl0 $y0
	set yl1 [expr {$yl0 + $cellheight}]
	
	$canvas create rect $xl0 $yl0 $xl1 $yl1 -fill $high -outline $gridcolor
	$canvas create text $xl1 [expr {$yl0 + $ch2}] -text "   high" -anchor w
	
	
	set yl0 [expr {$y0 + 2*$cellheight}]
	set yl1 [expr {$yl0 + $cellheight}]
	$canvas create rect $xl0 $yl0 $xl1 $yl1 -fill $low -outline $gridcolor
	$canvas create text $xl1 [expr {$yl0 + $ch2}] -text "   low" -anchor w

	


    }
    
    method GetColorFromValue {value max} {
	set factor [expr {double($value)/$max}]
	if {$factor > 1} {
	    set factor 1
	} elseif {$factor < 0} {
	    set factor 0
	}

	set r [expr {int($lR + $factor*$dR)}]
	set g [expr {int($lG + $factor*$dG)}]
	set b [expr {int($lB + $factor*$dB)}]
	
	return [format "#%4.4X%4.4X%4.4X" $r $g $b]
	
    }

    method UpdateRGB {} {
	my variable low high
	
	set R [string range $low 1 4]
	set G [string range $low 5 8]
	set B [string range $low 9 12]
	
	set lR [expr "0x$R"]
	set lG [expr "0x$G"]
	set lB [expr "0x$B"]
	
	
	set R [string range $high 1 4]
	set G [string range $high 5 8]
	set B [string range $high 9 12]
	
	set hR [expr "0x$R"]
	set hG [expr "0x$G"]
	set hB [expr "0x$B"]
	
	set dR [expr {$hR - $lR}]
	set dB [expr {$hB - $lB}]
	set dG [expr {$hG - $lG}]
	
    }

}


## set ct [crosstable -group $Area -color $Area]
## $ct configure -low yellow -high orange
