

namespace eval loon {
    namespace export navgraph

    proc navgraph {data {sep ":"} args} {

	set G [completegraph [dict keys $data]]
	set LG [linegraph {*}$G $sep]
	set LGnot [complement {*}$LG]


	set tt [::loon::loon_toplevel]

	set g [graph ${tt}.graph\
		   -nodes [lindex $LG 0]\
		   -from [lindex $LG 1]\
		   -to [lindex $LG 2]\
		   -isDirected [lindex $LG 3]\
		   -size 28]
	
	set gs [graphswitch ${tt}.graphswitch -activewidget $g]

	$gs add "3d transition" {*}$LG
	$gs add "4d transition" {*}$LGnot

	pack $g -side left -fill both -expand 1
	pack $gs -side left -fill y

	set navigator [$g navigator add -label "default navigator"]
	set context [$g navigator use $navigator context add geodesic2d -data $data -seperator $sep]
	set p [lindex [$g navigator use $navigator context use $context cget -command] 0]

	if {[llength $args] > 0} {
	    $p configure {*}$args
	}
	
	return [dict create graph $g navigator $navigator context $context plot $p]
	
    }    

}
