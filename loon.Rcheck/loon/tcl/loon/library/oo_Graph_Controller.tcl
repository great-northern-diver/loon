::oo::class create ::loon::classes::Graph_Controller {
    
    superclass	::loon::classes::Scatterplot_Controller

    
    ## graphInteractionMode: scatter or navigation
    ## orbit               : dict...
    ## navigation          : dict...
    ##     init      : TRUE/FALSE
    ##     navigator : navigator id
    ##     
    
    variable  navigation graphInteractionMode orbit activeNavigator_var\
	graphbinding

    constructor {args} {

	set graphInteractionMode scatter
	set navigation [dict create init FALSE isNewPath FALSE]
	set orbit [dict create init FALSE]

	set graphbinding ""

		
	next {*}$args
	
	
	
    }

    method init {} {
	my variable view

	
	next

	set canvas [$view getCanvas]
	
	set ns [self namespace]
	set tt [winfo toplevel $canvas]
	bind $canvas <Control-Double-1>       [list +${ns}::my DoubleClick]
	bind $tt <KeyPress-Shift_L>   [list +${ns}::my HighlightAdjacent]
	bind $tt <KeyPress-Shift_R>   [list +${ns}::my HighlightAdjacent]
	bind $tt <KeyRelease-Shift_L> [list +${ns}::my UnHighlightAdjacent]
	bind $tt <KeyRelease-Shift_R> [list +${ns}::my UnHighlightAdjacent]

	$canvas bind "model&&(navigator||navigatorLabel)" <Double-1> [list +${ns}::my Interchange4dContext2d]
	
    }

    

    method Interchange4dContext2d {} {
	my variable canvas model
	set tmp_navigator [lindex [uplevel #0 [list $canvas gettags current]] 3]

	foreach context [$model navigator use $tmp_navigator context ids] {
	    if {[$model navigator use $tmp_navigator context getType $context]\
		    in {"context2d" "geodesic2d"}} {
		## toggle interchange4d
		$model navigator use $tmp_navigator context use $context configure\
		    -interchange4d [::loon::listfns::booleanNot\
					[$model navigator use $tmp_navigator\
					     context use $context cget -interchange4d]]
	    }	    
	}
	
    }

    method setModel {Model} {
	my variable model

	if {$model ne "" && $graphbinding ne ""} {
	    catch {$model systembind state delete $graphbinding}
	}

	next $Model
	
	set activeNavigator_var [[info object namespace $Model]::my varname activeNavigator]
	
	set graphbinding [$Model systembind state add activeNavigator\
			      [list [self] updateNavigatorInteractionMode]]
		
    }

    
    method updateNavigatorInteractionMode {} {
	if {[llength [set $activeNavigator_var]] ne 0} {
	    set graphInteractionMode "navigation"
	    dict set navigation navigator [set $activeNavigator_var]
	} else {
	    set graphInteractionMode "scatter"
	}
    }
    
    method meta_b1 {x y meta} {

	my variable canvas view
	
	if {[my StopAnyAnimation]} {return}
		
	set tags [uplevel #0 [list $canvas gettags current]]
	
	
	if {$meta eq "Shift" && $graphInteractionMode eq "navigation"} {
	    
	    ## add to path
	    my variable model canvas
	    set navigator [dict get $navigation navigator]
	    set tags [uplevel #0 [list $canvas gettags current]]
	    
	    if {[lindex $tags 2] ne "point" } return
	    
	    set node [lindex [$model cget -nodes]\
			  [string range [lindex $tags 3] 4 end]]
	    
	    catch {$model navigator use $navigator addToPath $node}
	    my HighlightAdjacent
	    
	} elseif {$meta eq "Control" && $graphInteractionMode eq "navigation"} {

	    ## do nothing

	} elseif {$meta eq "Control" && [lindex $tags 2] eq "orbit"} {
	    
	    set graphInteractionMode orbit
	    
	    set i [string range [lindex $tags 3] 4 end]
	    
	    dict set orbit init TRUE
	    dict set orbit inode $i
	    set visual [$view getModelVisual]
	    dict set orbit cx [lindex [set ${visual}::curX] $i]
	    dict set orbit cy [lindex [set ${visual}::curY] $i]
	    
	} else {
	    next $x $y $meta
	}
    }
    
    method meta_b1_motion {x y meta} {
	my variable model 
	
	
	if {$meta eq "Shift" && $graphInteractionMode eq "navigation"} {
	    my button1_motion $x $y
	} elseif {$meta eq "Control" && $graphInteractionMode eq "orbit"} {

	    if {[dict get $orbit init]} {
		set cx [dict get $orbit cx]
		set cy [dict get $orbit cy]
		
		set dx [expr {$x - $cx}]
		set dy [expr {$cy - $y}]
		
		if {[expr {abs($dx) < 0.0001}]} {
		    set dx 0.0001
		}
		
		set atan [expr {atan(double(abs($dy))/abs($dx))}]
		
		if {$dx >= 0 && $dy >= 0} {
		    set angle $atan
		} elseif {$dx < 0 && $dy >= 0} {
		    set angle [expr {3.141593 - $atan}]
		} elseif {$dx < 0 && $dy <= 0} {
		    set angle [expr {3.141593 + $atan}]
		} else {
		    set angle [expr {6.283185 - $atan}]
		}
		
		
		uplevel #0 [list $model configure -orbitAngle $angle -which [dict get $orbit inode]]
		uplevel #0 [list update idletasks]
	    }
	} else {
	    next $x $y $meta
	}
    }
    
    method HighlightAdjacent {{adjacent ""}} {
	
	if {$graphInteractionMode ne "navigation"} {
	    return
	}
	
	my variable canvas model
	
	my  UnHighlightAdjacent
	if {[llength $adjacent] eq 0} {
	    set navigator [dict get $navigation navigator]
	    
	    set p [$model navigator use $navigator cget -proportion]
	    
	    set to [$model navigator use $navigator cget -to]
	    set from [$model navigator use $navigator cget -from]
	    
	#if {$p > 0} {
	#	set adjacent [list [lindex $from end] [lindex $to 0]]
	#   } else {
		if {$to eq ""} {
		    set adjacent [$model adjacent [lindex $from end]]
		} else {
		    set adjacent [$model adjacent [lindex $to end]]
		}
	#  }
	}
	
	set nodes [$model cget -nodes]
	foreach node $adjacent {
	    set i [lsearch -exact $nodes $node]
	    if {$i ne "-1"} {
		uplevel #0 [list $canvas itemconfigure "model&&point${i}"\
				-outline $::loon::Options(select-color) -width 4]
	    }
	}
    }
    
    method UnHighlightAdjacent {} {	
	## Remove highlighted adjeacend nodes
	my variable canvas
	uplevel #0 [list $canvas itemconfigure "model&&point" -outline ""]
    }
    
    
    method DoubleClick {} {
	my variable canvas model view
	
	set tags [uplevel #0 [list $canvas gettags current]]
	set tag2 [lindex $tags 2]
	if {$graphInteractionMode eq "navigation" &&\
		$tag2 eq "point" || $tag2 eq "navigatorPathEnd"} {
	    
	    ## Animate Navigator on path
	    
	    set navigator [dict get $navigation navigator]
	    set to [$model navigator use $navigator cget -to]
	    set from [$model navigator use $navigator cget -from]
	    
	    
	    if {$tag2 eq "navigatorPathEnd"} {
		set node [lindex $to end]
	    } else {
		set node [lindex [$model cget -nodes]\
			      [string range [lindex $tags 3] 4 end]]
	    }
	    
	    if {$node in $to} {
		$model navigator use $navigator walk forward $node	
	    } elseif {$node in $from} {
		$model navigator use $navigator walk backward $node	
	    }
	    
	} elseif {$graphInteractionMode eq "navigation" &&\
		      ([lindex $tags 2] eq "navigator" || [lindex $tags 2] eq "navigatorLabel")} {
	    set navigator [dict get $navigation navigator]
	    set to [$model navigator use $navigator cget -to]
	    set from [$model navigator use $navigator cget -from]
	    
	    $model navigator use $navigator configure\
		-from [lindex $from end] -to [lindex $to 0]
	}
	
    }

    method zoom {x y diff direction} {	

	
	if {$graphInteractionMode eq "navigation"} {
	    my variable model

	    if {[my StopAnyAnimation]} {return}
	    	    
	    set navigator [dict get $navigation navigator]
	    set to [$model navigator use $navigator cget -to]
	    set from [$model navigator use $navigator cget -from]
	    
	    set p [$model navigator use $navigator cget -proportion]
	    
	    
	    if {$to eq "" && $diff > 0} return
	    if {$p eq 0 && [llength $from] eq 1 && $diff < 0} return
	    
	    
	    set pincr [$model navigator use $navigator cget -scrollProportionIncrement]
	    
	    set newp [expr {$p + $diff * $pincr}]
	    
	    if {$newp > 1} {
		$model navigator use $navigator configure\
		    -proportion 0\
		    -from [concat $from [lindex $to 0]]\
		    -to [lrange $to 1 end]
		
	    } elseif {$newp < 0} {
		
		if {[llength $from] <= 1} {
		    $model navigator use $navigator configure\
			-proportion 0
		} else {
		    $model navigator use $navigator configure\
			-proportion 1\
			-from [lrange $from 0 end-1]\
			-to [concat [lindex $from end] $to]
		}
	    } else {
		$model navigator use $navigator configure -proportion $newp
	    }
	    
	    uplevel #0 [list update idletasks]
	
	} else {
	    next $x $y $diff $direction
	}
    }
    
    ## drag navigators ?
    method button1_press {x y} {
	my variable canvas model view

	if {[my StopAnyAnimation]} {return}
		
	set tags [uplevel #0 [list $canvas gettags current]]
	
	#puts "Button Press"

	switch -- [lindex $tags 2] {
	    navigator -
	    navigatorLabel {		
		set graphInteractionMode navigation

		set navigator [lindex $tags 3]

		$model configure -activeNavigator $navigator
		
		set from [$model navigator use $navigator cget -from]

		if {$from eq ""} {
		    dict set navigation navigator $navigator
		    return 
		}
				
		dict set navigation init TRUE
		dict set navigation navigator $navigator
		dict set navigation x0 $x
		dict set navigation y0 $y
		
		my InitNavigatorDrag $navigator
		
		my save_coords $x $y
	    }
	    navigatorPathEnd {
		if {$graphInteractionMode eq "navigation"} {
		    ## start path
		    set navigator [dict get $navigation navigator]
		    
		    set node [lindex [$model navigator use $navigator cget -to] end]
		    
		    # Jump
		    $model navigator use $navigator configure\
			-from $node -to "" -proportion 0
		} else {

		    $model configure -activeNavigator ""
		    #uplevel #0 [list $canvas itemconfigure "model&&navigator"\
		    #		    -outline black\
		    #		    -width 1]
		    
		    set graphInteractionMode scatter
		    next $x $y
		}
	    }
	    point {
		if {$graphInteractionMode eq "navigation"} {
		    ## start path
		    set navigator [dict get $navigation navigator]
		    
		    set node [lindex [$model cget -nodes]\
				  [string range [lindex $tags 3] 4 end]]
		    
		    
		    # Jump
		    $model navigator use $navigator configure\
			-from $node -to "" -proportion 0
		} else {

		    $model configure -activeNavigator ""
		    
		    set graphInteractionMode scatter
		    next $x $y
		}
	    }
	    default  {
		if {$graphInteractionMode eq "navigation"} {
		    $model configure -activeNavigator ""		    
		} 
		
		set graphInteractionMode scatter
		next $x $y
		
	    }	
	}
    }

    method button1_motion {x y} {	
	my variable model canvas

	## deal with glitch issue
	set wx [winfo pointerx $canvas]
	set wy [winfo pointery $canvas]
	set rx [winfo rootx $canvas]
	set ry [winfo rooty $canvas]
	
	set x [expr {$wx-$rx}]
	set y [expr {$wy-$ry}]

	
	switch -- $graphInteractionMode {
	    navigation {
		if {[dict get $navigation init]} {
		    my MoveNavigator $x $y
		}
	    }
	    default {
		next $x $y
	    }
	}
	
    }
    
    method EdgeSelectionGuides {} {
	
    }
    method ResetEdgeSelectionGuides {} {
	my variable canvas
	if {[dict exists $navigation highlightedEdgeId]} {
	    uplevel #0 [list $canvas itemconfigure\
			    [dict get $navigation highlightedEdgeId]\
			    -fill [dict get $navigation highlightedEdgeColor]\
			    -width [dict get $navigation highlightedEdgeWidth]]
	    
	    dict unset navigation highlightedEdgeId
	    dict unset navigation highlightedEdgeColor
	    dict unset navigation highlightedEdgeWidth	       
	}
	if {[dict exists $navigation line]} {
	    uplevel #0 [list $canvas delete [dict get $navigation line]]
	    dict unset navigation line
	}
	if {[dict exists $navigation circle]} {
	    uplevel #0 [list $canvas delete [dict get $navigation circle]]
	    dict unset navigation circle
	}
    }
    
    method button1_release {x y} {

	dict set orbit init FALSE
	
	if {[dict get $navigation init]} {
	    dict set navigation init FALSE
	    
	    my variable model canvas
    
	    ## Move navigator back
	    set navigator [dict get $navigation navigator]
	    set to [$model navigator use $navigator cget -to]
	    set proportion [$model navigator use $navigator cget -proportion]
	    
	    if {[dict get $navigation inEdgeSelectionMode]} {
		#move navigator back to node
		my ResetEdgeSelectionGuides
		
		uplevel #0 [list $canvas move current\
				[expr {[dict get $navigation x0] -$x}]\
				[expr {[dict get $navigation y0] -$y}]]
		
	    }  elseif {$proportion eq 0 && [llength $to] eq 1 &&\
			   [dict get $navigation isNewPath]} { 
		$model navigator use $navigator configure -to "" -proportion 0
		
	    } elseif {$proportion eq 1} {
		set from [$model navigator use $navigator cget -from]
		
		$model navigator use $navigator configure\
		    -from [concat $from [lindex $to 0]]\
		    -to [lrange $to 1 end]\
		    -proportion 0
	    }
	    
	    dict set navigation isNewPath FALSE
	    
	    my UnHighlightAdjacent

	    uplevel #0 [list update idletasks]
	} else {
	    if {$graphInteractionMode ne "navigation"} {
		set graphInteractionMode scatter
	    }
	    
	    next $x $y
	}	
    }
    
    
    method InitNavigatorDrag {navigator} {
	my variable canvas model

	set from [$model navigator use $navigator cget -from]
	set from_node [lindex $from end]
	
	set to [$model navigator use $navigator cget -to]
	set to_node [lindex $to 0]
	
	set p [$model navigator use $navigator cget -proportion]
	
	#puts "init navigator drag: $from_node ($p) $to_node"


	if {$p > 0 && $p < 1} {
	    ## navigator on edge
	    
	    dict set navigation inEdgeSelectionMode FALSE

	    set id [my FindEdgeId $from_node $to_node]
	    
	    set vecs [my NormEdgeVec $id $from_node]
	    dict set navigation curEdgeX0 [lindex $vecs 0]
	    dict set navigation curEdgeY0 [lindex $vecs 1]
	    dict set navigation curEdgeX [lindex $vecs 2] 
	    dict set navigation curEdgeY [lindex $vecs 3] 
	    dict set navigation curEdgeMagnitude [lindex $vecs 4] 
	} else {
	    ## Select edge
	    dict set navigation inEdgeSelectionMode TRUE
	    dict set navigation adjacent {}
	    dict set navigation adjEdgeX {}
	    dict set navigation adjEdgeY {}
	    dict set navigation adjEdgeId {}
	    dict set navigation adjEdgeX0 {}
	    dict set navigation adjEdgeY0 {}
	    dict set navigation adjEdgeX {}
	    dict set navigation adjEdgeY {}
	    dict set navigation adjEdgeMagnitude {}
	    
	    set isDirected [$model cget -isDirected]	    
	    set nodes [$model cget -nodes]
	    
	    if {[llength $to] > 0 } {
		## select forward or backwards
		set adj [list [lindex $from end-1] $to_node]
	    } else {
		## add new edge to path
		## or delete previous
		set adj [$model adjacent $from_node]
	    }
	    my HighlightAdjacent $adj
	    
	    ## now find firection vectors
	    ## and add selection guides
	    set numAdj 0
	    foreach node $adj {
		if {[$model isEdgeSeen $from_node $node]} {
		    ## Find Edge
		    set id [my FindEdgeId $from_node $node $isDirected]
		    incr numAdj
		    
		    dict lappend navigation adjacent $node
		    dict lappend navigation adjEdgeId $id
		    
		    ## now calculate direction vector of edge
		    set vecs [my NormEdgeVec $id $from_node]
		    dict lappend navigation adjEdgeX0 [lindex $vecs 0]
		    dict lappend navigation adjEdgeY0 [lindex $vecs 1]
		    
		    ## Node center is only used if there are adacent nodes
		    dict set navigation xFromCenter [lindex $vecs 0]
		    dict set navigation yFromCenter [lindex $vecs 1]
		    
		    
		    dict lappend navigation adjEdgeX [lindex $vecs 2] 
		    dict lappend navigation adjEdgeY [lindex $vecs 3] 
		    dict lappend navigation adjEdgeMagnitude [lindex $vecs 4]
		    
		    ##puts "--\nid: $id, ([expr {double($xe)/$mag}], [expr {double($ye)/$mag}])"
		}
	    }
	    if {$numAdj eq 0} {
		dict set navigation noAdjacent TRUE
	    } else {
		dict set navigation noAdjacent FALSE
	    
		## Add Circle
		set x0 [dict get $navigation xFromCenter]
		set y0 [dict get $navigation yFromCenter]
		
		if {[dict exists $navigation circle]} {
		    uplevel #0 [list $canvas delete [dict get $navigation circle]]
		}
		dict set navigation circle\
		    [uplevel #0 [list $canvas create oval\
				     [expr {$x0-28}]\
				     [expr {$y0-28}]\
				     [expr {$x0+28}]\
				     [expr {$y0+28}]\
				     -outline "gray60" -width 2]]
	    }
	    
	}	
    }
    
   
 
    method MoveNavigator {x y} {
	if {[dict get $navigation init]} {
	    my variable model view canvas mouse_x mouse_y
	    
	    ## either set -to or increase proportion 
	    set navigator [dict get $navigation navigator]
	    
	    set to [$model navigator use $navigator cget -to]
	    set to_node [lindex $to 0]
	    
	    if {[dict get $navigation inEdgeSelectionMode]} {
		if {[dict get $navigation noAdjacent]} {
		    ## do nothing
		} else {
		    ## select edge
		    
		    uplevel #0 [list $canvas move current\
				    [expr {$x-$mouse_x}] [expr {$y-$mouse_y}]]
		    
		    set x0 [dict get $navigation xFromCenter]
		    set y0 [dict get $navigation yFromCenter]
		    
		    ## add direction line and circle
		    if {![dict exists $navigation line]} {
			dict set navigation line\
			    [uplevel #0 [list $canvas create line $x0 $y0 $x $y\
					     -fill grey60 -width 2]]
		    } else {
			uplevel #0 [list $canvas coords [dict get $navigation line]\
					$x0 $y0 $x $y]
		    }
		    
		    if {![dict exists $navigation circle]} {
			dict set navigation circle\
			    [uplevel #0 [list $canvas create oval\
					     [expr {$x0-28}]\
					     [expr {$y0-28}]\
					     [expr {$x0+28}]\
					     [expr {$y0+28}]\
					     -outline "gray60" -width 2]]
			
		    } else {
			uplevel #0 [list $canvas coords [dict get $navigation circle]\
					[expr {$x0-28}]\
					[expr {$y0-28}]\
					[expr {$x0+28}]\
					[expr {$y0+28}]]
			
		    }
		    
		    ##Find which edge is the closest
		    
		    ## if drag radius exceeds a certain distance
		    set dist [expr {pow($x-$x0,2)+pow($y-$y0,2)}]
		    if {$dist < 0.001} {set dist 0.001}
		    
		    ## decide on "-to" for the navigator
		    set xe [expr {double($x-$x0)/sqrt($dist)}]
		    set ye [expr {double($y-$y0)/sqrt($dist)}]
		    
		    set minAngle ""
		    set minI ""
		    set i 0
		    foreach exe [dict get $navigation adjEdgeX]\
			eye [dict get $navigation adjEdgeY] {
			    
			    set angle [expr {acos($xe*$exe+$ye*$eye)}]
			    if {$minAngle eq "" || $angle < $minAngle} {
				set minAngle $angle
				set minI $i
			    } 
			    incr i
			}
		    #puts "$minAngleNode"
		    
		    ## Select edge to color
		    ## Temporarily
		    if {[dict exists $navigation highlightedEdgeId]} {
			uplevel #0 [list $canvas itemconfigure\
					[dict get $navigation highlightedEdgeId]\
					-fill [dict get $navigation highlightedEdgeColor]\
					-width [dict get $navigation highlightedEdgeWidth]]
			
			dict unset navigation highlightedEdgeId
			dict unset navigation highlightedEdgeColor
			dict unset navigation highlightedEdgeWidth
		    }
		    
		    if {$minAngle ne ""} {
			set minAngleEdgeId [lindex [dict get $navigation adjEdgeId] $minI]
			if {$minAngleEdgeId ne ""} {
			    dict set navigation highlightedEdgeId $minAngleEdgeId
			    dict set navigation highlightedEdgeColor\
				[uplevel #0 [list $canvas itemcget $minAngleEdgeId -fill]]
			    dict set navigation highlightedEdgeWidth\
				[uplevel #0 [list $canvas itemcget $minAngleEdgeId -width]]
			    
			    uplevel #0 [list $canvas itemconfigure $minAngleEdgeId\
					    -fill $::loon::Options(select-color) -width 12]
			}
		    }
		    
		    ## if navigator has been dragged far enough, set -to if one available
		    if {$dist > 800} {
			## remove visual aids
			my ResetEdgeSelectionGuides 
			my UnHighlightAdjacent
			


			## Now copy vector data so that it is a smooth trasition
			## between selection and traversal
			if {$minI ne ""} {
			    dict set navigation inEdgeSelectionMode FALSE

			    set from [$model navigator use $navigator cget -from]
			    set from_node [lindex $from end]
			    set from_node1 [lindex $from end-1]
			    
			    set new_to [lindex [dict get $navigation adjacent] $minI]
			    
			    if {$new_to eq $from_node1} {
				## go backwards
				set id [my FindEdgeId $from_node1 $from_node]
				set vecs [my NormEdgeVec $id $from_node1]
				dict set navigation curEdgeX0 [lindex $vecs 0]
				dict set navigation curEdgeY0 [lindex $vecs 1]
				dict set navigation curEdgeX [lindex $vecs 2] 
				dict set navigation curEdgeY [lindex $vecs 3] 
			    } else {
				## go forwards
				dict set navigation curEdgeX0\
				    [lindex [dict get $navigation adjEdgeX0] $minI]
				dict set navigation curEdgeY0\
				    [lindex [dict get $navigation adjEdgeY0] $minI]
				dict set navigation curEdgeX\
				    [lindex [dict get $navigation adjEdgeX] $minI]
				dict set navigation curEdgeY\
				    [lindex [dict get $navigation adjEdgeY] $minI]
			    }
			    dict set navigation curEdgeMagnitude\
				[lindex [dict get $navigation adjEdgeMagnitude] $minI] 
			    
			    
			    ## either forwards backwards or new node
			    if {$new_to eq $from_node1} {
				set to [concat $from_node $to]
				set from [lrange $from 0 end-1]
				set p 1
			    } elseif {$new_to eq $to_node} {
				## do nothing
				set p 0
			    } else {
				## add new node to path
				set to $new_to
				set p 0
				## to know whether it should be deleted when button gets
				## released and bullet is again on from_node
				dict set navigation isNewPath TRUE
			    }
			    uplevel #0 [list $model navigator use $navigator\
					    configure -from $from -to $to -proportion $p]
			    
			}
		    }
		} 
	    }
	    
	    if {![dict get $navigation inEdgeSelectionMode]} {
		
		## traverse on edge
		set dx [expr {$x - [dict get $navigation curEdgeX0]}]
		set dy [expr {$y - [dict get $navigation curEdgeY0]}]
		
		## project on edge
		set mag [expr {$dx*[dict get $navigation curEdgeX]+\
				   $dy*[dict get $navigation curEdgeY]}]
		
		set proportion [expr {double($mag)/[dict get $navigation curEdgeMagnitude]}]
		
		if {$proportion > 1} {
		    set proportion 1
		} elseif {$proportion < 0} {
		    set proportion 0
		}
		## I set -to state for the case the -to state needs to be initialized
		uplevel #0 [list $model navigator use [dict get $navigation navigator]\
				configure -proportion $proportion]
	    }
	    
	    my save_coords $x $y
	    uplevel #0 [list update idletasks]
	}
	
    }

    method FindEdgeId {from to {isDirected ""}} {
	my variable canvas model
	if {$to eq "" || $from eq ""} {
	    error "empty string: to is $to and from is $from"
	}

	## find edge on canvas
	set ids [uplevel #0 [list $canvas find withtag\
				 "layer&&model&&edge&&${from}&&${to}"]]
	
	if {$isDirected eq ""} {
	    set isDirected [$model cget -isDirected] 
	}

	set id ""
	if {$isDirected} {
	    foreach ide $ids {
		set tags [uplevel #0 [list $canvas gettags $ide]]
		if {[lindex $tags 4] eq $from && [lindex $tags 5] eq $to} {
		    set id $ide
		}
	    }
	} else {
	    if {[llength $ids]>1} {
		error "multiple items on the canvas qualified for the edge $from $node"
	    } else {
		set id $ids
	    }
	}
	if {$id eq ""} {
	    error "did not find edge $from $node"
	}
	return $id	
    }

    ## from node is origin
    method NormEdgeVec {id from} {
	my variable canvas
	set coords [uplevel #0 [list $canvas coords $id]]
	set tags [uplevel #0 [list $canvas gettags $id]]
	if {[lindex $tags 4] eq $from} {
	    set eX0 [lindex $coords 0]
	    set eY0 [lindex $coords 1]
	    
	    set xe [expr {[lindex $coords 2] - [lindex $coords 0]}]
	    set ye [expr {[lindex $coords 3] - [lindex $coords 1]}]
	} else {
	    set eX0 [lindex $coords 2]
	    set eY0 [lindex $coords 3]
	    
	    set xe [expr {[lindex $coords 0] - [lindex $coords 2]}]
	    set ye [expr {[lindex $coords 1] - [lindex $coords 3]}]
	}
	
	set mag [expr {sqrt($xe*$xe+$ye*$ye)}]

	return [list $eX0 $eY0 [expr {double($xe)/$mag}] [expr {double($ye)/$mag}] $mag]
    }

    method StopAnyAnimation {} {
	my variable model

	set animationStop FALSE	
	foreach navigator [$model navigator ids] {
	    if {[$model navigator use $navigator inAnimation]} {
		$model navigator use $navigator stopAnimation
		set animationStop TRUE
	    }
	}
	return $animationStop
    }
    
}
