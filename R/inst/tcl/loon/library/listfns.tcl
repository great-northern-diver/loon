
## \brief several elementwise function on lists.
namespace eval loon::listfns {

    namespace export interleave

    proc isNumeric {vec} {
	foreach x $vec {
	    if {![string is double $x]} {
		return 0
	    }
	}
	return 1
    }

    proc isInteger {vec} {
	foreach x $vec {
	    if {![string is integer $x]} {
		return 0
	    }
	}
	return 1
    }

    proc isPositiveInteger {vec} {
	foreach x $vec {
	    if {![string is integer $x] || $x < 0} {
		return 0
	    }
	}
	return 1
    }


    proc isPositiveNumber {vec} {
	foreach x $vec {
	    if {![string is double $x] || $x < 0} {
		return 0
	    }
	}
	return 1
    }


    proc isBoolean {vec} {
	foreach x $vec {
	    if {![string is boolean $x]} {
		return 0
	    }
	}
	return 1
    }

    proc isImage {vec} {
	foreach x $vec {
	    if {$x ni [image names]} {
		return 0
	    }
	}
	return 1
    }

    proc isColor {vec} {
	foreach x $vec {
	    if {[catch {winfo rgb . $x}]} {
		return 0
	    }
	}
	return 1
    }

    proc toHexcolor {vec} {
	set hexcolor {}
	if {[llength $vec] eq 1} {
	    set hexcolor [eval format #%04X%04X%04X [winfo rgb . {*}$vec]]
	} else {
	    foreach c $vec {
		lappend hexcolor [eval format #%04X%04X%04X [winfo rgb . $c]]
	    }
	}
	return $hexcolor
    }

    proc toNumeric {vec} {
	set out {}
	set val -1

	foreach x $vec {
	    if {![info exists arr($x)]} {
		set arr($x) [incr val]
	    }
	    lappend out $arr($x)
	}
	return $out
    }

    ## lsort -unique is still faster
    proc unique {vec} {

        set d [dict create]

        foreach x $vec {
            if {![dict exists $d $x]} {
                dict set d $x TRUE
            }
        }

        return [dict keys $d]
    }

    ## map input to colors
    proc mapColor {vec} {
	set groups [unique $vec]

	set nGroups [llength $groups]

	if {$::loon::Options(colors-palette) eq "ggplot2"} {
	    set colors [::loon::hcl::hue_pal $nGroups]
	} else {

	    ## Use available colors
	    set nColors [llength $::loon::Options(colors)]

	    if {$nGroups > $nColors} {
		if {$::loon::Options(colors-palette) ne "hcl"} {
		    puts "warning: need more colors than are\
                          available in current color palette.\
                          Using hcl colors for the current color encoding."
		}
		set colors [::loon::hcl::hue_mem_pal $nGroups\
				{*}$::loon::Options(colors-palette-hcl)]
	    } else {
		set colors [lrange $::loon::Options(colors) 0 [expr {$nColors-1}]]
	    }

	}

	foreach group $groups col $colors {
	    set table($group) $col
	}

	set col {}
	foreach v $vec {
	    lappend col $table($v)
	}

	return $col
    }

    proc lseq {from to} {
	set out {}
	if {$from <= $to} {
	    for {set i $from} {$i <= $to} {incr i} {lappend out $i}
	} else {
	    for {set i $from} {$i >= $to} {incr i -1} {lappend out $i}
	}
	return $out
    }

    proc lseq2 {from to n} {

	set step [expr {($to - $from)/($n-1.0)}]
	set out $from
	set nm1 [expr {$n - 1}]
	for {set i 1} {$i < $nm1} {incr i} {
	    lappend out [expr {$from + $i*$step}]
	}
	lappend out $to
	return $out
    }

    # arguments list of booleans
    proc which {L} {
	set which [list]
	set i 0

	foreach element $L {
	    if {$element} {
		lappend which $i
	    }
	    incr i
	}
	return $which
    }
    proc subset {L which} {
	set res {}

	foreach w $which {
	    lappend res [lindex $L $w]
	}
	return $res
    }

    proc subsetLogical {L which} {
	set res {}

	foreach e $L w $which {
	    if {$w} {
		lappend res $e
	    }
	}
	return $res
    }


    proc min {vec {index "all"}} {
	if {$index eq "all"} {
	    set min [lindex $vec 0]
	    foreach x $vec {
		if {$x < $min} {set min $x}
	    }
	} else {
	    set min [lindex $vec [lindex $index 0]]
	    foreach i $index {
		if {[lindex $vec $i]< $min} {set min [lindex $vec $i]}
	    }
	}
	return $min
    }

    proc max {vec {index "all"}} {
	if {$index eq "all"} {
	    set max [lindex $vec 0]
	    foreach x $vec {
		if {$x > $max} {set max $x}
	    }
	} else {
	    set max [lindex $vec [lindex $index 0]]
	    foreach i $index {
		if {[lindex $vec $i] > $max} {set max [lindex $vec $i]}
	    }
	}
	return $max
    }


    proc MinMax {vec {index "all"}} {
	if {$index eq "all"} {
	    set min [lindex $vec 0]
	    set max $min

	    foreach x $vec {
		if {$x < $min} {set min $x}
		if {$x > $max} {set max $x}
	    }

	} else {
	    set min [lindex $vec [lindex $index 0]]
	    set max $min

	    foreach i $index {
		set x [lindex $vec $i]
		if {$x < $min} {set min $x}
		if {$x > $max} {set max $x}
	    }
	}
	return [list $min $max]
    }

    # http://wiki.tcl.tk/1354
    proc sum L {expr [join $L +]+0}

    proc scale01 {vec {min ""} {max ""}} {

	if {$min eq "" || $max eq ""} {
	    set mm [MinMax $vec]
	    set min [lindex $mm 0]
	    set max [lindex $mm 1]
	}

	set range [expr {$max - $min}]

	if {$range eq 0} {
	   set range 1
	}

	set svec [list]
	foreach x $vec {
	    lappend svec [expr {($x - $min)/double($range)}]
	}

	return $svec
    }


    proc scale {vec {mean ""} {stdev ""}} {
	if {$mean eq ""} {
	    set mean [mean $vec]
	}
	if {$stdev eq ""} {
	    set stdev [stdev $vec $mean]
	}
	set out {}

	foreach x $vec {
	    lappend out [expr {double($x-$mean)/$stdev}]
	}
	return $out
    }

    proc mean {vec} {
	set n [llength $vec]
	set sum 0
	foreach x $vec {
	    set sum [expr {$sum+$x}]
	}
	return [expr {double($sum)/$n}]
    }

    proc median {vec} {
       if {[set len [llength $vec]] % 2} then {
         return [lindex [lsort -real $vec] [expr {($len-1) / 2}]]
       } else {
         return [expr {([lindex [set sorted [lsort -real $vec]] [expr {($len/2) - 1}]] \
                        + [lindex $sorted [expr {$len/2}]]) / 2.0}]
       }
    }

    proc stdev {vec {mean ""}} {
	set n [llength $vec]
	if {$mean eq ""} {
	    set mean [mean $vec]
	}
	set ssum 0
	foreach x $vec {
	    set ssum [expr {$ssum + pow($x - $mean,2)}]
	}
	return [expr {sqrt(double($ssum)/($n - 1))}]
    }

    # http://wiki.tcl.tk/12848
    proc lapply {list FUN} {
	set res {}
	foreach element $list {
	    lappend res [{*}$FUN $element]
	}
	return $res
    }

    proc all {list} {
	foreach element $list {
	    if {$element} {} else { return 0 }
	}
    	return 1
    }

    proc any {list} {
	foreach element $list {
	    if {$element} { return 1}
	}
    	return 0
    }

    proc booleanAs01 {list} {
	set res {}
	foreach element $list {
	    if {$element} {
		lappend res 1
	    } else {
		lappend res 0
	    }
	}
	return $res
    }

    proc booleanAsTRUEFALSE {list} {
	set res {}
	foreach element $list {
	    if {$element} {
		lappend res TRUE
	    } else {
		lappend res FALSE
	    }
	}
	return $res
    }

    proc booleanNot {B} {
	set n [llength $B]

	if {$n eq 1} {
	    if {$B} {
		return 0
	    } else {
		return 1
	    }
	} else {
	    set x [list]
	    foreach b $B {
		if {$b} {
		    lappend x 0
		} else {
		    lappend x 1
		}
	    }
	    return $x
	}
    }

    # A \ B
    proc setDiff {A B} {
	set out [list]
	foreach a $A {
	    if {$a ni $B} {
		lappend out $a
	    }
	}
	return $out
    }

    ##
    proc setIntersection {A B} {
	set out {}
	set A [lsort -unique $A]
	set B [lsort -unique $B]

	if {[llength $A] > [llength $B]} {
	    foreach b $B {
		if {$b in $A} {
		    lappend out $b
		}
	    }
	} else {
	    foreach a $A {
		if {$a in $B} {
		    lappend out $a
		}
	    }
	}
	return $out
    }

    ## get range of x and y if list format is {x1 y1 x2 y2 ...}
    proc xyrange {xy} {
	set xmin [lindex $xy 0]
	set xmax $xmin
	set ymin [lindex $xy 1]
	set ymax $ymin

	foreach {x y} $xy {
	    if {$x < $xmin} {
		set xmin $x
	    } elseif {$x > $xmax} {
		set xmax $x
	    }

	    if {$y < $ymin} {
		set ymin $y
	    } elseif {$y > $ymax} {
		set ymax $y
	    }
	}

	return [list $xmin $xmax $ymin $ymax]
    }


    proc drange {x} {
	set tmp [::loon::listfns::MinMax $x]
	return [expr {[lindex $tmp 1] - [lindex $tmp 0]}]
    }

    proc extractAndRemoveOption {listname name {default ""}} {
	set lst [uplevel 1 [list set $listname]]
	set i [lsearch -exact $lst $name]

	if {$i ne -1} {
	    set ip1 [expr {$i + 1}]
	    set option [lindex $lst $ip1]
	    uplevel 1 [list set $listname [lreplace $lst $i $ip1]]
	} else {
	    set option $default
	}
	return $option
    }

    ## (a b c) (1 2 3) (I II III) > (a 1 I b 2 II c 3 III)
    proc interleave {args} {
	if {[llength $args] eq 0} {
	    error "wrong number of arguments: interleave list list ?list? ... ?list?"
	}
	set p [llength $args]
	set n [llength [lindex $args 0]]

	set out {}
	for {set j 0} {$j < $n} {incr j} {
	    for {set i 0} {$i < $p} {incr i} {
		lappend out [lindex [lindex $args $i] $j]
	    }
	}
	return $out
    }

    ## e.g ljoin {1 2 3} {a b c} > {1a 2b 3c}
    proc ljoin {A B} {
	set out {}
	foreach a $A b $B {
	    lappend out "${a}${b}"
	}
	return $out
    }


    ## from R jitter function
    proc jitter {x {factor 1} {amount ""}} {
	if {[llength $x] eq 0} {
	    return $x
	}
	if {![isNumeric $x]} {
	    error "x must be numeric"
	}

	set z [drange $x]
	if {$z eq 0} {
	    set z [min $x]
	}
	if {$z eq 0} {
	    set z 1
	}
	if {$amount eq ""} {
	    set xx [lsort -unique [round $x [expr {3-floor(log10($z))}]]]
	    set d {}
	    ## R diff() function
	    foreach ixx0 [lreplace $xx end end] ixx1 [lreplace $xx 0 0] {
		lappend d [expr {$ixx1 - $ixx0}]
	    }

	    if {[llength $d] ne 0} {
		set d [min $d]
	    } elseif {$xx != 0} {
		set d [expr {$xx/10.0}]
	    } else {
		set d [expr {$z / 10.0}]
	    }

	    set amount [expr {$factor/5.0*abs($d)}]

	} elseif {$amount eq 0} {
	    set amount [expr {$factor * $z/50.0}]
	}

	set out {}
	foreach e $x {
	    lappend out [expr {$e + 2.0*rand()*$amount - $amount}]
	}
	return $out
    }

    proc round {x {decimalplaces 0}} {
	set out {}

	set f [expr {pow(10, $decimalplaces)}]

	foreach e $x {
	    lappend out [expr {double(round($f*$e))/$f}]
	}
	return $out
    }

    ## project (x,y) orthogonally on vec
    proc comp2d {x y vec} {
	if {[llength $vec] ne 2} {
	    error "vec needs to have 2 elements"
	}

	set nvec [norm $vec]

	set v1 [lindex $nvec 0]
	set v2 [lindex $nvec 1]

	set out {}
	foreach xe $x ye $y {
	    lappend out [expr {$xe*$v1+$ye*$v2}]
	}
	return $out
    }

    proc norm {vec} {
	set ssum 0
	foreach x $vec {
	    set ssum [expr {$ssum + pow($x,2)}]
	}
	set len [expr {sqrt(double($ssum))}]
	set out {}
	foreach x $vec {
	    lappend out [expr {double($x)/$len}]
	}
	return $out
    }

    proc dot {v1 v2} {
        if {[llength $v1] ne [llength $v1]} {
            error "v1 needs to have same dimension as v2"
        }
        set out 0.0
        foreach x $v1 y $v2 {
            set out [expr {$out + $x*$y}]
        }
        return $out
    }

    proc transpose { matrix } {
        set transposed {}
        set i 0
        foreach col [lindex $matrix 0] {
           set resultRow {}
           foreach row $matrix {
               lappend resultRow [lindex $row $i]
           }
           lappend transposed $resultRow
           incr i
        }
        return $transposed
    }

    proc matmul { m1 m2 } {
        set result {}
        set m2t [transpose $m2]
        foreach row1 $m1 {
          set resultRow {}
          foreach row2 $m2t {
             lappend resultRow [dot $row1 $row2]
          }
          lappend result $resultRow
        }
        return $result
    }

    # mcsplit --
    # http://code.activestate.com/recipes/68386-multi-character-split/
    #   Splits a string based using another string
    #
    # Arguments:
    #   str       string to split into pieces
    #   splitStr  substring
    #   mc        magic character that must not exist in the orignal string.
    #             Defaults to the NULL character.  Must be a single character.
    # Results:
    #   Returns a list of strings
    #
    proc mcsplit "str splitStr {mc {\x00}}" {
	return [split [string map [list $splitStr $mc] $str] $mc]
    }

    # fourier transformation
    proc fourierTrans {vec n} {

      set k [llength $vec]
      set indices [::loon::listfns::lseq 1 $k]
      set pi 3.141592653
      set ts [::loon::listfns::lseq2  -$pi $pi $n]

      set fourier {}

      foreach t $ts {

        set series {}

        foreach i $indices {

          if {$i == 1} {
            set at [expr 1/[tcl::mathfunc::sqrt 2]]
          } else {
            set floor  [expr [tcl::mathfunc::floor [expr $i / 2] ] - 1]
            set coef [tcl::mathfunc::pow 2 $floor]
            if {[expr $i % 2]} {
               # cos
               set at [tcl::mathfunc::cos [expr $coef * $t]]
            } else {
               # sin
               set at [tcl::mathfunc::sin [expr $coef * $t]]
            }

          }

          set v [lindex $vec [expr $i - 1]]

          lappend series [expr $at * $v]
       }

       lappend fourier [::loon::listfns::sum $series]
    }

  return $fourier
  }

  # keep two decimals
  # e.g. [round2 {1.234 23.2345}]
  # return 1.23, 23.23
  proc round2 {vec} {

    set xs {}

	foreach v $vec {
	  # keep two decimals
	  set x [expr {double(round(100*$v))/100}]
	  lappend xs $x
	}

	return $xs
  }

  # get the odd index of a vector
  # e.g. [getOdd {11 12 13 14 15 16}]
  # will return 11 13 15
  proc getOdd {vec} {

      set newvec {}
      set l [llength $vec]
      for {set i 0} {$i < $l} {incr i} {

        if {$i % 2 == 0} {
          lappend newvec [lindex $vec $i]
        }
      }

      return $newvec
  }

  # get the even index of a vector
  # e.g. [getEven {11 12 13 14 15 16}]
  # will return 12 14 16
  proc getEven {vec} {

      set newvec {}
      set l [llength $vec]
      for {set i 0} {$i < $l} {incr i} {

        if {$i % 2 == 1} {
          lappend newvec [lindex $vec $i]
        }
      }

      return $newvec
  }

  proc coordsSumOfSquare {vec} {

    set newvec {}
    set l [llength $vec]

    if {$l % 2 == 1} {
       error "the length of coords is odd"
    }

    set n [expr $l/2]
    for {set i 0} {$i < $n} {incr i} {
      set x [lindex $vec [expr $i * 2]]
      set y [lindex $vec [expr $i * 2 + 1]]
      lappend newvec [tcl::mathfunc::sqrt [expr $x**2 + $y**2]]
    }

    return $newvec
  }
}

