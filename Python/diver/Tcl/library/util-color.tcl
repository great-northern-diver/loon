# util-color.tcl --
#
# Code here was copied from Jeffrey Hobbs 
# https://github.com/tcltk/widget/blob/master/library/util-color.tcl
#
# 
#
# Copyright (c) 1998 Jeffrey Hobbs
#
# See the file "license.terms" for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

namespace eval loon::color {
    

    # rgb2dec --
    #
    # Turns #rgb into 3 elem list of decimal vals.
    #
    # Arguments:
    # c                The #rgb hex of the color to translate
    # Results:
    # Returns a #RRGGBB or #RRRRGGGGBBBB color
    #
    proc rgb2dec c {
	set c [string tolower $c]
	if {[regexp {^#([0-9a-f])([0-9a-f])([0-9a-f])$} $c x r g b]} {
	    # double'ing the value make #9fc == #99ffcc
	    scan "$r$r $g$g $b$b" "%x %x %x" r g b
	} else {
	    if {![regexp {^#([0-9a-f]+)$} $c junk hex] || \
		    [set len [string length $hex]]>12 || $len%3 != 0} {
		return -code error "bad color value \"$c\""
	    }
	    set len [expr {$len/3}]
            scan $hex "%${len}x%${len}x%${len}x" r g b
	}
	return [list $r $g $b]
    }

    # dec2rgb --
    #
    # Takes a color name or dec triplet and returns a #RRGGBB color.
    # If any of the incoming values are greater than 255,
    # then 16 bit value are assumed, and #RRRRGGGGBBBB is
    # returned, unless $clip is set.
    #
    # Arguments:
    # r                red dec value, or list of {r g b} dec value or color name
    # g                green dec value, or the clip value, if $r is a list
    # b                blue dec value
    # clip        Whether to force clipping to 2 char hex
    # Results:
    # Returns a #RRGGBB or #RRRRGGGGBBBB color
    #
    proc dec2rgb {r {g 0} {b UNSET} {clip 0}} {
	if {![string compare $b "UNSET"]} {
	    set clip $g
	    if {[regexp {^-?(0-9)+$} $r]} {
		foreach {r g b} $r {break}
	    } else {
		foreach {r g b} [winfo rgb . $r] {break}
	    }
	}
	set max 255
	set len 2
	if {($r > 255) || ($g > 255) || ($b > 255)} {
	    if {$clip} {
		set r [expr {$r>>8}]; set g [expr {$g>>8}]; set b [expr {$b>>8}]
	    } else {
		set max 65535
		set len 4
	    }
	}
	return [format "#%.${len}X%.${len}X%.${len}X" \
		    [expr {($r>$max)?$max:(($r<0)?0:$r)}] \
		    [expr {($g>$max)?$max:(($g<0)?0:$g)}] \
		    [expr {($b>$max)?$max:(($b<0)?0:$b)}]]
    }

    # shade --
    #
    # Returns a shade between two colors
    #
    # Arguments:
    # orig        start #rgb color
    # dest        #rgb color to shade towards
    # frac        fraction (0.0-1.0) to move $orig towards $dest
    # Results:
    # Returns a shade between two colors based on the
    #
    proc shade {orig dest frac} {
	if {$frac >= 1.0} { return $dest } elseif {$frac <= 0.0} { return $orig }
	foreach {origR origG origB} [rgb2dec $orig] \
	    {destR destG destB} [rgb2dec $dest] {
		set shade [format "\#%02x%02x%02x" \
			       [expr {int($origR+double($destR-$origR)*$frac)}] \
			       [expr {int($origG+double($destG-$origG)*$frac)}] \
			       [expr {int($origB+double($destB-$origB)*$frac)}]]
		return $shade
	    }
    }

    # complement --
    #
    # Returns a complementary color
    # Does some magic to avoid bad complements of grays
    #
    # Arguments:
    # orig        start #rgb color
    # Results:
    # Returns a complement of a color
    #
    proc complement {orig {grays 1}} {
	foreach {r g b} [rgb2dec $orig] {break}
	set R [expr {(~$r)%256}]
	set G [expr {(~$g)%256}]
	set B [expr {(~$b)%256}]
	if {$grays && abs($R-$r) < 32 && abs($G-$g) < 32 && abs($B-$b) < 32} {
	    set R [expr {($r+128)%256}]
	    set G [expr {($g+128)%256}]
	    set B [expr {($b+128)%256}]
	}
	return [format "\#%02x%02x%02x" $R $G $B]
    }

    # hsv2rgb --
    #
    # Convert hsv to rgb
    #
    # Arguments:
    # h                hue
    # s                saturation
    # v                value
    # Results:
    # Returns an rgb triple from hsv
    #
    proc hsv2rgb {h s v} {
	if {$s <= 0.0} {
	    # achromatic
	    set v [expr {int($v)}]
	    return "$v $v $v"
	} else {
	    set v [expr {double($v)}]
	    if {$h >= 1.0} { set h 0.0 }
	    set h [expr {6.0 * $h}]
	    set f [expr {double($h) - int($h)}]
	    set p [expr {int(256 * $v * (1.0 - $s))}]
	    set q [expr {int(256 * $v * (1.0 - ($s * $f)))}]
	    set t [expr {int(256 * $v * (1.0 - ($s * (1.0 - $f))))}]
	    set v [expr {int(256 * $v)}]
	    switch [expr {int($h)}] {
		0 { return "$v $t $p" }
		1 { return "$q $v $p" }
		2 { return "$p $v $t" }
		3 { return "$p $q $v" }
		4 { return "$t $p $v" }
		5 { return "$v $p $q" }
	    }
	}
    }

    proc hls2rgb {h l s} {
	# Posted by frederic.bonnet@ciril.fr
	# h, l and s are floats between 0.0 and 1.0, ditto for r, g and b
	# h = 0 => red
	# h = 1/3 => green
	# h = 2/3 => blue

	set h6 [expr {($h-floor($h))*6}]
	set r [expr { $h6 <= 3 ? 2-$h6
		      : $h6-4}]
	set g [expr { $h6 <= 2 ? $h6
		      : $h6 <= 5 ? 4-$h6
		      : $h6-6}]
	set b [expr { $h6 <= 1 ? -$h6
		      : $h6 <= 4 ? $h6-2
		      : 6-$h6}]
	set r [expr {$r < 0.0 ? 0.0 : $r > 1.0 ? 1.0 : double($r)}]
	set g [expr {$g < 0.0 ? 0.0 : $g > 1.0 ? 1.0 : double($g)}]
	set b [expr {$b < 0.0 ? 0.0 : $b > 1.0 ? 1.0 : double($b)}]

	set r [expr {(($r-1)*$s+1)*$l}]
	set g [expr {(($g-1)*$s+1)*$l}]
	set b [expr {(($b-1)*$s+1)*$l}]
	return [list $r $g $b]
    }

}; 