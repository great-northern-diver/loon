

::oo::class create ::loon::classes::LabelsVisual {

    superclass ::loon::classes::Visual


    variable title xlabel ylabel swap\
	idTitle idXlabel idYlabel color\
	marginBottom marginTop marginLeft
    
    constructor {args} {
	
	set color black
	set title ""
	set xlabel ""
	set ylabel ""
	
	set marginBottom 40
	set marginLeft 40
	set marginTop 60
	
	foreach var {idTitle idXlabel idYlabel} {
	    set $var ""
	}
	
	next {*}$args
	
    }
    
    method setColor {Color} {
	set color $Color
	my updateLabels
    }

    method setLabels {Title Xlabel Ylabel} {
	set title $Title
	set xlabel $Xlabel
	set ylabel $Ylabel
	
	my updateLabels
    }
    
    method setMargins {bottom left top {right 0}} {
	set marginBottom $bottom
	set marginLeft $left
	set marginTop $top

	my updateLabels
    }

    method InfoDebug {args} {
	next title xlabel ylabel swap {*}$args
    }
    
    method clear {} {
	next
	set idTitle ""
	set idXlabel ""
	set idYlabel ""
    }

    method redraw {} {
	my variable canvas visualid isVisible
	
	my clear

	if {$isVisible} {set state normal} else {set state hidden}
	
	set idTitle [uplevel #0 [list $canvas create text 0 0\
				     -text $title\
				     -tag [list loon $visualid title]\
				     -state $state\
				     -fill $color\
				     -font $::loon::Options(font-title)\
				     -justify "center"]]

	set idXlabel [uplevel #0 [list $canvas create text 0 0\
				      -text $xlabel\
				      -tag [list loon $visualid xlabel]\
				      -fill $color\
				      -font $::loon::Options(font-xlabel)\
				      -state $state\
				      -justify "center"]]

	set idYlabel [uplevel #0 [list $canvas create text 0 0\
				      -text $ylabel\
				      -tag [list loon $visualid ylabel]\
				      -font $::loon::Options(font-ylabel)\
				      -fill $color\
				      -state $state\
				      -justify "center"]]
    
	my updateCoords
    }

    method updateLabels {} {
	my variable canvas
	uplevel #0 [list $canvas itemconfigure $idTitle -text $title -fill $color]
	uplevel #0 [list $canvas itemconfigure $idXlabel -text $xlabel -fill $color]
	uplevel #0 [list $canvas itemconfigure $idYlabel -text $ylabel -fill $color]
    }
    
    method updateCoords {} {
	my variable canvas map isVisible
	
	if {$idXlabel eq ""} {my redraw}
	
	if {$isVisible} {
	    set canvas_width [winfo width $canvas]
	    set canvas_height [winfo height $canvas]
	    
	    set vp [$map getViewport]
	    set vpx0 [lindex $vp 0]
	    set vpy0 [lindex $vp 1]
	    set vpx1 [lindex $vp 2]
	    set vpy1 [lindex $vp 3]
	    set swap [$map getSwap]
	    
	    set xmiddle [expr {($vpx0 + $vpx1)/2.0}]
	    set ymiddle [expr {($vpy0 + $vpy1)/2.0}]

	    set ytitle [expr {$marginTop/2.0}]

	    uplevel #0 [list $canvas coords $idTitle $xmiddle $ytitle]
	    
	    set y_bottom [expr {$canvas_height - $marginBottom/2.0}]
	    set x_left [expr {$marginLeft/2.0}]
	    
	    if {$swap} {
		uplevel #0 [list $canvas coords $idYlabel $xmiddle $y_bottom]
		uplevel #0 [list $canvas coords $idXlabel $x_left $ymiddle]
		
		uplevel #0 [list $canvas itemconfigure $idXlabel -angle 90]
		uplevel #0 [list $canvas itemconfigure $idYlabel -angle 0]
	    } else {
		uplevel #0 [list $canvas coords $idYlabel $x_left $ymiddle]
		uplevel #0 [list $canvas coords $idXlabel $xmiddle $y_bottom]

		uplevel #0 [list $canvas itemconfigure $idXlabel -angle 0]
		uplevel #0 [list $canvas itemconfigure $idYlabel -angle 90]
	    }
	}
    }
    

    
}
