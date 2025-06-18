# image_scale.tcl
# Pure-Tcl image scaling for loon (compatible with Tcl 8.6 and 9.0)
# Source: Cleaned and enhanced version of the wiki.tcl.tk #11196 reference

namespace eval ::loon {}

if {![catch {package require ImageScale}]} {
    set ::loon::Options(image_scale) "::imagescale::image_scale"
} else {
    set ::loon::Options(image_scale) "::loon::image_scale"
}

proc ::loon::image_scale {src newx newy {dest ""}} {
    if {$newx < 1 || $newy < 1} {
        set newx 5
        set newy 5
    }

    set mx [image width $src]
    set my [image height $src]

    if {$dest eq ""} {
        set dest [image create photo]
    }
    $dest configure -width $newx -height $newy

    # Check if integer zoom is possible
    if {$newx % $mx == 0 && $newy % $my == 0} {
        set ix [expr {$newx / $mx}]
        set iy [expr {$newy / $my}]
        $dest copy $src -zoom $ix $iy
        return $dest
    }

    set ny 0
    set ytot $my

    for {set y 0} {$y < $my} {incr y} {
        foreach {pr pg pb} [$src get 0 $y] {break}
        set row {}
        set thisrow {}

        set nx 0
        set xtot $mx

        for {set x 1} {$x < $mx} {incr x} {
            while {$xtot <= $newx} {
                lappend row [format "#%02x%02x%02x" $pr $pg $pb]
                lappend thisrow $pr $pg $pb
                incr xtot $mx
                incr nx
            }

            foreach {r g b} [$src get $x $y] {break}

            set xtot [expr {$xtot - $newx}]
            set rn $xtot
            set rp [expr {$mx - $xtot}]
            set xr 0
            set xg 0
            set xb 0

            while {$xtot > $newx} {
                incr xr $r
                incr xg $g
                incr xb $b
                set xtot [expr {$xtot - $newx}]
                incr x
                foreach {r g b} [$src get $x $y] {break}
            }

            set tr [expr {int(($rn*$r + $xr + $rp*$pr) / $mx)}]
            set tg [expr {int(($rn*$g + $xg + $rp*$pg) / $mx)}]
            set tb [expr {int(($rn*$b + $xb + $rp*$pb) / $mx)}]
            foreach var {tr tg tb} {if {[set $var] > 255} {set $var 255}}

            lappend row [format "#%02x%02x%02x" $tr $tg $tb]
            lappend thisrow $tr $tg $tb
            incr xtot $mx
            incr nx

            set pr $r; set pg $g; set pb $b
        }

        while {$nx < $newx} {
            lappend row [format "#%02x%02x%02x" $r $g $b]
            lappend thisrow $r $g $b
            incr nx
        }

        if {[info exists prevrow]} {
            set nrow {}

            while {$ytot <= $newy} {
                $dest put [list $prow] -to 0 $ny
                incr ytot $my
                incr ny
            }

            set ytot [expr {$ytot - $newy}]
            set rn $ytot
            set rp [expr {$my - $ytot}]

            while {$ytot > $newy} {
                set ytot [expr {$ytot - $newy}]
                incr y
                continue
            }

            foreach {pr pg pb} $prevrow {r g b} $thisrow {
                set tr [expr {int(($rn*$r + $rp*$pr) / $my)}]
                set tg [expr {int(($rn*$g + $rp*$pg) / $my)}]
                set tb [expr {int(($rn*$b + $rp*$pb) / $my)}]
                lappend nrow [format "#%02x%02x%02x" $tr $tg $tb]
            }

            $dest put [list $nrow] -to 0 $ny
            incr ytot $my
            incr ny
        }

        set prevrow $thisrow
        set prow $row
    }

    while {$ny < $newy} {
        $dest put [list $row] -to 0 $ny
        incr ny
    }

    return $dest
}
