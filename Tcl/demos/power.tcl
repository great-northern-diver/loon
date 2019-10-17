
package require loon
package require Img

namespace import loon::*

proc powerfun {x lambda} {
    if {abs($lambda) < 0.01} {
        return [lmap val $x {expr {log($val)}}]
    } else {
        return [lmap val $x {expr {(pow($val, $lambda) - 1.0)/$lambda}}]
    }
}

proc updatePlot {topl args} {
    set lambda_x [${topl}.scalex get]
    set lambda_y [${topl}.scaley get]
    
    set x [set ::${topl}.xdata]
    set y [set ::${topl}.ydata]
    
    set new_x [powerfun $x $lambda_x]
    set new_y [powerfun $y $lambda_y]
    
    set p ${topl}.plot
    $p configure -x $new_x -y $new_y
    $p scaleto world
}

proc power {x y {from -5} {to 5} args} {
    # create a toplevel window
    set i 0
    while {[winfo exists .l${i}]} {
        incr i
    }
    set tl [toplevel .l${i}]

    ## save original data
    uplevel #0 [list set ${tl}.xdata $x]
    uplevel #0 [list set ${tl}.ydata $y]
     
    ## create a loon plot
    set p [::loon::plot ${tl}.plot -x $x -y $y {*}$args]
    
    ## create scales    
    set sx [scale ${tl}.scalex -orient horizontal\
            -from $from -to $to -resolution 0.1]
    set sy [scale ${tl}.scaley -orient vertical\
            -from $to -to $from -resolution 0.1]
    
    $sx set 1
    $sy set 1
    
    ## layout
    grid $sy -row 0 -column 0 -sticky ns
    grid $p -row 0 -column 1 -sticky nswe
    grid $sx -row 1 -column 1 -sticky we
    
    grid columnconfigure $tl 1 -weight 1
    grid rowconfigure $tl 0 -weight 1
    
    
    $sx configure -command [list updatePlot $tl]
    $sy configure -command [list updatePlot $tl]
    
    return $p
}


dict with ::loon::data::mammals {
   power $body $brain -5 5\
   -xlabel "body" -ylabel "brain" -title "mammal brain vs body weight"\
   -itemLabel $name -showItemLabels TRUE -size 6
}

