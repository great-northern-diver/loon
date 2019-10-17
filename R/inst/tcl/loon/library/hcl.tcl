
namespace eval loon::hcl {

#    namespace export hcl2rgb
    ## Converted from hcl2rgb in R source: colors.c


    
    variable DEG2RAD 0.01745329251994329576


    ##  D65 White Point    
    variable WHITE_X 95.047
    variable WHITE_Y 100.000
    variable WHITE_Z 108.883
    variable WHITE_u 0.1978398
    variable WHITE_v 0.4683363
    
    proc hcl {h c l} {
	variable DEG2RAD
	variable WHITE_X
	variable WHITE_Y
	variable WHITE_Z
	variable WHITE_u
	variable WHITE_v

	set h [expr {double($h)}]
	set c [expr {double($c)}]
	set l [expr {double($l)}]
	
       	if {$l <= 0.0} {
	    return {0 0 0}; # r g b
	}
	
	# Step 1 : Convert to CIE-LUV
	set h [expr {$DEG2RAD * $h}]
	set L $l
	set U [expr {$c * cos($h)}]
	set V [expr {$c * sin($h)}]

	# Step 2 : Convert to CIE-XYZ

	if {$L <= 0 && $U eq 0 && $V eq 0} {
	    set X 0
	    set Y 0
	    set Z 0
	} else {
	    if {$L > 7.999592} {
		set Y [expr {$WHITE_Y * pow(($L + 16)/116, 3)}]
	    } else {
		set Y [expr {$WHITE_Y * ($L / 903.3)}]
	    }
	    
	    set u [expr {$U / (13 * $L) + $WHITE_u}]
	    set v [expr {$V / (13 * $L) + $WHITE_v}]
	    set X [expr {9.0 * $Y * $u / (4 * $v)}]
	    set Z [expr {-$X / 3 - 5 * $Y + 3 * $Y / $v}]

	    
	}
	
	# Step 4 : CIE-XYZ to sRGB	

	set rgb [list\
		    [gtrans [expr {( 3.240479 * $X - 1.537150 * $Y - 0.498535 * $Z) / $WHITE_Y}]]\
		    [gtrans [expr {(-0.969256 * $X + 1.875992 * $Y + 0.041556 * $Z) / $WHITE_Y}]]\
		    [gtrans [expr {( 0.055648 * $X - 0.204043 * $Y + 1.057311 * $Z) / $WHITE_Y}]]]

	#puts "rgb = $rgb"
	return [format #%02X%02X%02X {*}$rgb]
    }

    ## GAMMA 2.4
    variable GAMMA 2.4
    proc gtrans {u} {
	variable GAMMA
	if {$u > 0.00304} {
	    set x [expr {1.055 * pow($u, (1/$GAMMA)) - 0.055}]
	} else {
	    set x [expr {12.92 * $u}]
	}

	set c [expr {int(255 * $x + .5)}]

	while {$c < 0} {
	    set c 0
	}

	while {$c > 255} {
	    set c 255
	}
	
	return $c
    }

    proc hue_pal {n {h {15 375}} {c 100} {l 65} {h_start 0} {direction 1}} {

	set h1 [lindex $h 0]
	set h2 [lindex $h 1]
	
	if {[expr {fmod($h2-$h1, 360)}] < 1} {
	    set h2 [expr {$h2 - 360.0/$n}]
	}

	set hues [lmap hue [::loon::listfns::lseq2 $h1 $h2 $n] {
	    expr {fmod($hue + $h_start, 360) * double($direction)}
	}]

	return [lmap hue $hues {hcl $hue $c $l}]	
	
    }


    ## Need a palette that if n changes to n+1 then
    ## elements 1:n do not change for the new color palette
    ##
    ## On circle produce sequence in degrees:
    ## 0 * 180.0 * 90 270 * 45 225 135 315 *  22.5 202.5 112.5 292.5 ...
 
    
    
    
    variable memoisedColors ""
    variable angles ""
    proc hue_mem_pal {n {c 100} {l 65} {hue_offset 0}} {
	variable memoisedColors
	variable angles
	
	if {$n <= 0} {
	    error "n needs to be > 0."
	}

	if {[llength $memoisedColors] < $n} {

	    set angles [getAngles $n]
	    
	    set memoisedColors\
		[lmap angle $angles\
		     {hcl [expr {$angle+$hue_offset}] $c $l}]
	}
	
	return [lrange $memoisedColors 0 [expr {$n - 1}]]
    }


    proc getAngles {n} {
	if {$n eq 1} {
	    return 0
	} elseif {$n eq 2} {
	    return [list 0 180]
	} else {
	    set k 0
	    while { [expr {pow(2,$k)}] < $n} {
		incr k
	    }
	    set nseries [expr {pow(2,$k)}]
	    
	    
	    set odds 1
	    for {set i 3} {$i < [expr {$nseries/2}]} {incr i 2} {
		lappend odds $i
	    }
	    
	    set odds_sep [recursive_divide [list $odds]] 
	    
	    set n2 [expr {int($nseries/2)}]
	    set out {}
	    
	    foreach o $odds_sep {
		lappend out [expr {360*$o/double($nseries)}]
		lappend out [expr {360*($o + $n2)/double($nseries)}]
	    }
	    return [lrange [concat [getAngles $n2] $out] 0 [expr {$n-1}]]
	}	
    }

    #getAngles 1
    #getAngles 2
    #getAngles 4
    #getAngles 8
    #getAngles 32
    

    proc recursive_divide {num_set} {
	set new_num_set [list]
	set out [list]

	foreach nums $num_set {
	    set n [llength $nums]
	    if {$n eq 1} {
		lappend out $nums
	    } else {
		if {[expr {$n%2}] eq 1} {
		    set i [expr {($n+1)/2 - 1}]
		    lappend out [lindex $nums $i]
		    lappend new_num_set [lrange $nums 0 [expr {$i-1}]]
		    lappend new_num_set [lrange $nums [expr {$i+1}] end]
		} else {
		    lappend out [lindex $nums 0]
		    lappend new_num_set [lrange $nums 1 end]
		}
	    }	    
	}
	#puts "out=$out"
	#puts "new_num_set=$new_num_set"
	if {[llength $new_num_set] eq 0} {
	    return $out
	} else {
	    return [concat $out [recursive_divide $new_num_set]]
	}		
    }

    ## recursive_divide {{1 3 5 7 9 11 13 15}}


    
    proc hclColorPaletteDialog {args} {
	set tt [::loon::loon_toplevel]

	wm title $tt "Hcl color palette"
	catch {wm attributes $tt -type dialog}

	set colors [HclGUI new $tt {*}$args]
	
    }
    
    
    ::oo::class create HclGUI {

	variable n hue_start chroma luminance\
	    canvas pad side color_order\
	    choose_order

	constructor {path {N 15} {Chroma 112} {Luminance 51} {Hue_start 318}} {

	    set n $N

	    set color_order [loon::listfns::lseq 0 [expr {$n - 1}]]

	    set choose_order [dict create init FALSE]
	    
	    set hue_start $Hue_start
	    set chroma $Chroma
	    set luminance $Luminance

	    set pad 30
	    set side 60	    

	    set scale_n [tk::scale ${path}.n\
			     -from 2 -to 40 -resolution 1\
			     -orient horizontal -label "n:"\
			     -variable [my varname n]\
			     -command "[self namespace]::my Change_n"]

	    set scale_hue_start [tk::scale ${path}.hue_start\
				     -from 0 -to 360 -resolution 1\
				     -orient horizontal -label "hue start:"\
				     -variable [my varname hue_start]\
				     -command "[self namespace]::my Redraw"]

	    set scale_chroma [tk::scale ${path}.chroma\
				  -from 0 -to 400 -resolution 1\
				  -orient horizontal\
				  -label "chroma: max. value depending on hue and luminance"\
				  -variable [my varname chroma]\
				  -command "[self namespace]::my Redraw"]

	    set scale_luminance [tk::scale ${path}.luminance\
				     -from 0 -to 100 -resolution 1\
				     -orient horizontal -label "luminance:"\
				     -variable [my varname luminance]\
				     -command "[self namespace]::my Redraw"]
	    
	    set canvas [tk::canvas ${path}.canvas -bg white]

	    
	    
	    
	    foreach w [list $scale_n $scale_hue_start $scale_chroma\
			   $scale_luminance] {
		grid $w -sticky ew
	    }

	    grid $canvas -sticky nsew

	    set fbuttons [frame ${path}.controls]
	    set badd [button ${fbuttons}.ok -text add -command "[self namespace]::my Add"]
	    set bcancel [button ${fbuttons}.cancel -text cancel]

	    grid $fbuttons -sticky ew
	    pack $badd $bcancel -side right
	    

	    grid columnconfigure $path 0 -weight 1
	    grid rowconfigure $path 4 -weight 1

	    bind $canvas <Configure> "[self namespace]::my Redraw"

	    
	    
	}

	method Change_n {args} {
	    
	    set color_order [loon::listfns::lseq 0 [expr {$n - 1}]]	    

	    my Redraw
	    
	}

	method Add {} {
	    puts [loon::hcl::hue_pal $n\
		      {0 360} $chroma $luminance $hue_start]
	}
	

	## Eventually move and drag color boxes
	method Grab_col {x y} {}

	method Move_col {x y} {}

	method Set_col {x y} {}

	
	
	method Redraw {args} {
	    $canvas delete all

	    set w [winfo width $canvas]
	    set h [winfo height $canvas]


	    set colors [loon::hcl::hue_pal $n\
			    {0 360} $chroma $luminance $hue_start]
	    
	    set x0 $pad
	    set y0 $pad
	    set x1 [expr {$pad + $side}]
	    set y1 [expr {$pad + $side}]
	    

	    
	    foreach i $color_order {

		set col [lindex $colors $i]
		
		$canvas create rect $x0 $y0 $x1 $y1 -fill $col -outline ""
		

		set x0 [expr {$x1 + $pad}]
		set x1 [expr {$x0 + $side}]

		if {[expr {$x1 + $pad}] > $w} {
		    set x0 $pad
		    set x1 [expr {$pad + $side}]
		    set y0 [expr {$y1 + $pad}]
		    set y1 [expr {$y0 + $side}]		    
		}
	    }
	}

	
	
	
    }
    
    
}
