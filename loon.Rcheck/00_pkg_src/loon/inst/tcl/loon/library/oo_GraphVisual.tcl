

::oo::class create ::loon::classes::GraphVisual { 
    
    superclass ::loon::classes::ScatterplotVisual
    
    variable idEdges nodes_var showEdge_var iftedges_var colorEdge_var p_var p\
	isDirected_var from_var to_var foreground_var activeNavigator_var\
	orbitDistance_var orbitAngle_var showOrbit_var idOrbits dxOrbit dyOrbit anchorOrbit\
	navigators navigatorBinding	
    

    constructor {Layerobj args} {
	my variable modelns
	
	set idEdges ""
	set idOrbits ""
	set p 0
	
	set navigators [dict create]
	
	## get the graph model namespace
	set modelns [set [info object namespace $Layerobj]::modelns]
	foreach state {nodes showEdge iftedges colorEdge p isDirected from to\
		       orbitDistance orbitAngle showOrbit foreground activeNavigator} {
	    set ${state}_var [uplevel #0 [list ${modelns}::my varname $state]]
	}
	
	next $Layerobj {*}$args
	
	## Add all navigators
	foreach navigator [${modelns}::my navigator ids] {
	    my AddNavigator $navigator
	}
	#my updateNavigatorCoords
    }
    

    method MakeStateBinding {} {
	my variable modelns
	set navigatorBinding\
	    [uplevel #0 [list ${modelns}::my systembind navigator add\
			     all [list [self] navigatorupdate %nav %e]]]
	
	next
    }

    
    destructor {
	my variable modelns 
	if {$navigatorBinding ne ""} {
	    catch {${modelns}::my systembind navigator delete $navigatorBinding}
	}
	
	dict for {nav d} $navigators {
	    catch {${modelns}::my navigator use $nav systembind\
		       state delete [dict get $d stateBinding]}
	}
	
	next


    }
    
    
    ## Layer update is graph update
    method LayerUpdateDict {events} {

	if {[dict exists $events "n"] || [dict exists $events "p"] || \
		[dict exists $events "nodes"] ||  \
		[dict exists $events "from"] || [dict exists $events "to"] || \
		[dict exists $events "glyph"] || [dict exists $events "showOrbit"]} {
	    my variable n_var n
	    
	    set n [set $n_var]
	    set p [set $p_var]

	    my redraw
	    return
	} else {
	    
	    if {[dict exists $events "activeEdge"] || [dict exists $events "active"]} {
		my updateActive
	    }
	    
	    if {[dict exists $events "color"] ||\
		    [dict exists $events "colorEdge"] ||\
		    [dict exists $events "selected"] ||\
		    [dict exists $events "foreground"] } {
		my recolor
	    }
	    
	    
	    if {[dict exists $events "tag"]} {
		my updateTags
	    }
	    
	    
	    if {[dict exists $events "size"]  || \
		    [dict exists $events "x"] || \
		    [dict exists $events "y"] || \
		    [dict exists $events "xTemp"] ||\
		    [dict exists $events "yTemp"]} {
		my updateCoords
	    }

	    if {[dict exists $events "isDirected"]} {
		my updateEdgeAttributes
	    }
	    
	    if {[dict exists $events "orbitAngle"] || [dict exists $events "orbitDistance"]} {
		my updateOrbits
	    }

	    if {[dict exists $events "activeNavigator"]} {
		my setActiveNavigator
	    }


	    
	}

	
	
    }
    
    method clear {} {
	next
	
	set idEdges ""
	set idOrbits ""
    }
    
    method redraw {} {
	my variable canvas isVisible ids visualid

	my clear
	
	if {!$isVisible} { return }
	
	## edges
	set idEdges {}
	if {[set $p_var] > 0} {
	    for {set i 0} {$i < [set $p_var]} {incr i} {
		lappend idEdges [uplevel #0 [list $canvas create line 0 0 0 0]]
	    }
	    my updateEdgeAttributes
	}
	
	## navigators
	foreach nav [dict keys $navigators] {
	    [dict get $navigators $nav visual] redraw   
	}
	
	## create orbit labels
	if {[set $showOrbit_var]} {
	    set idOrbits {}	
	    foreach node [set $nodes_var] {
		lappend idOrbits [uplevel #0 [list $canvas create text 0 0 -text $node]]
	    }
	    my updateOrbits FALSE
	}

	## nodes and tags
	next FALSE

	my setActiveNavigator
	
	uplevel #0 [list $canvas raise navigatorPathEnd $visualid]
	uplevel #0 [list $canvas raise navigator $visualid]
	uplevel #0 [list $canvas raise navigatorLabel $visualid]    
	uplevel #0 [list $canvas raise orbit $visualid]    
	
    }
    
    
    method updateCoords {} {
	
	next 
	
	my updateOrbitCoords
	my updateEdgeCoords
 	my updateNavigatorCoords
    }
    

    method updateEdgeCoords {} {
	my variable canvas isVisible curX curY isCurXYinvalidated n_var
	
	if {!$isVisible || [set $n_var] eq 0} { return }
	
	if {$isCurXYinvalidated} {
	    my updateCurXY
	}
		
	if {[llength $idEdges] ne [set $p_var]} {
	    error "data consistency for points"
	}
	
	foreach {ifrom ito} [set $iftedges_var] id $idEdges {
	    uplevel #0 [list $canvas coords $id\
			    [lindex $curX $ifrom] [lindex $curY $ifrom]\
			    [lindex $curX $ito] [lindex $curY $ito]]
	}
	
    }
    

    method recolor {} {
	my variable canvas visualid
	
	next
	my recolorEdges

	## update orbit color to foreground
	uplevel #0 [list $canvas itemconfigure\
			"layer&&${visualid}&&orbit" -fill [set $foreground_var]]
    }
    
    method recolorEdges {} {
	my variable canvas

	foreach id $idEdges col [set $colorEdge_var] {
	    uplevel #0 [list $canvas itemconfigure $id -fill $col]
	}
    }
    
    method updateTags {} {
	my variable canvas isVisible visualid n_var
	
	## Nodes
	next 
	
	# if {!$isVisible || [set $n_var] eq 0 || [set $p_var] eq 0} { return }
	
	## Edges
	set i 0
	foreach id $idEdges from [set $from_var] to [set $to_var] {
	    set ttags [list layer $visualid edge "item$i" $from $to]
	    uplevel #0 [list $canvas itemconfigure $id -tag $ttags]	    
	    incr i
	}

	## Orbits
	set i 0
	foreach id $idOrbits {
	    set ttags [list layer $visualid orbit "item$i"]
	    uplevel #0 [list $canvas itemconfigure $id -tag $ttags]
	    incr i
	}
	
	
    }

    method updateOrbits {{updateCoords TRUE}} {
   	set dxOrbit {}
	set dyOrbit {}
	set anchorOrbit {}

	set dist [expr {sqrt([set $orbitDistance_var])}]
#	puts "angles: [set $orbitAngle_var]"
	foreach angleUser [set $orbitAngle_var] {
	    set angle [expr {fmod($angleUser, 6.283185)}]
	    if {$angle < 0} {
		set angle [expr {6.283185 - $angle}]
	    }

	    lappend dxOrbit [expr {$dist*cos($angle)}]
	    lappend dyOrbit [expr {-$dist*sin($angle)}]
	    if {$angle <= 1.570796} {
		lappend anchorOrbit "sw"
	    } elseif {$angle <=  3.141593} {
	       	lappend anchorOrbit "se"
	    } elseif {$angle <=  4.712389} {
		lappend anchorOrbit "ne"
	    } else {
		lappend anchorOrbit "nw"
	    }
	}
#	puts "update orbits: $dxOrbit, $dyOrbit"
	if {$updateCoords} {
	    my updateOrbitCoords
	}
    }
	


    method updateOrbitCoords {} {
	my variable canvas isCurXYinvalidated curX curY isVisible n_var ids
	
#	puts "update orbit coords: $dxOrbit, $dyOrbit"

	if {[llength $dxOrbit] ne [llength $curX]} {
	    my updateOrbits FALSE           
	}
	
	if {![set $showOrbit_var] || !$isVisible || [set $n_var] eq 0} {return}
	
	if {$isCurXYinvalidated} {
	    my updateCurXY
	}
	
	foreach id $idOrbits idnode $ids x $curX y $curY\
	    dx $dxOrbit dy $dyOrbit anchor $anchorOrbit {
		
		set bbox [uplevel #0 [list $canvas coords [lindex $idnode 0]]]
		
		set ddx [expr {([lindex $bbox 2] -[lindex $bbox 0])/2.0}]
		set ddy [expr {([lindex $bbox 1] -[lindex $bbox 3])/2.0}]
		
		switch -- $anchor {
		    sw {
			
		    }
		    se {
			set ddx [expr {-1*$ddx}]
		    }
		    nw {
			set ddy [expr {-1*$ddy}]
		    }
		    ne {
			set ddy [expr {-1*$ddy}]
			set ddx [expr {-1*$ddx}]
		    }
		}
		uplevel #0 [list $canvas coords $id [expr {$x+$ddx+$dx}] [expr {$y+$ddy+$dy}]]
		uplevel #0 [list $canvas itemconfigure $id -anchor $anchor]
	}

    }
    
    
    method updateEdgeAttributes {} {
	my variable canvas
	
	if {[set $isDirected_var]} {
	    set arrowArgs [list -arrow last -arrowshape [list 16 20 6]]
	} else {
	    set arrowArgs [list -arrow none]
	}
	
	foreach id $idEdges {
	    uplevel #0 [list $canvas itemconfigure $id {*}$arrowArgs]
	}

    }

    method updateActive {} {
	my variable isVisible canvas n_var active_var

	next 
	#if {!$isVisible || [set $n_var] eq 0 || $p eq 0} { return }
	
	foreach id $idEdges show [set $showEdge_var] {
	    if {$show} {
		uplevel #0 [list $canvas itemconfigure $id -state normal]
	    } else {
		uplevel #0 [list $canvas itemconfigure $id -state hidden]
	    }
	}
	
	if {[llength $idOrbits] > 0} {
	    foreach id $idOrbits active [set $active_var] {
		if {$active} {
		    uplevel #0 [list $canvas itemconfigure $id -state normal]
		} else {
		    uplevel #0 [list $canvas itemconfigure $id -state hidden]
		}
	    }
	}
		
    }

    
    method updateZoomPan {oldPanX oldPanY oldZoomX oldZoomY} {
	
	my updateCoords
	
    }



    ##
    ## Work With Navigatos
    ##
    ##
    
    ## add, delete
    method navigatorupdate {navigator event} {
	my variable modelns visualid canvas map
       
	switch -- $event {
	    add {
		my AddNavigator $navigator
		my updateNavigatorCoords $navigator
	    }
	    delete {
		if {[dict exists $navigators $navigator]} {
		    # not necessary because it is already destroyed
		    #${modelns}::my navigator use $navigator systembind state delete\
		    # 	[dict get $navigators $navigator stateBinding]

		    # why does this delete all items?
		    ##[dict get $navigators $navigator visual] destroy
		    dict unset navigators $navigator
		}
		$canvas delete layer&&model&&$navigator
	    }
	}
    }

   
    
 
    method navigatorconfigured {navigator events} {
	my variable isVisible n_var

	#	if {!$isVisible || [set $n_var] eq 0} { return }

	if {"label" in $events} {
	    [dict get $navigators $navigator visual] relabel
	}
	
	if {"proportion" in $events || "from" in $events || "to" in $events} {
	    my updateNavigatorCoords $navigator
	}

	if {"color" in $events} {
	    [dict get $navigators $navigator visual] recolor
	}
	
	
    }
    

    
    ## Add all navigator visuals when setting a new plotModel
    method NavigatorInit {} {
	if {$plotModel ne ""} {
	    set navigators [$plotModel navigator ids]
	    foreach nav $navigators {
		my AddNavigator $nav
	    }
	}

    }


       
    method AddNavigator {navigator {isWorldview FALSE}} {
	my variable modelns visualid canvas map
	## visualid is the same as the one from the graph
	## as they are on the same layer
	
	#puts "Graph Visual add Navigator: $navigator"

	set navObj [${modelns}::my navigator getObject $navigator]

	if {$isWorldview} {
	    set navVisual [::loon::classes::NavigatorWorldviewVisual new\
			       $navigator $navObj $visualid $canvas $map]
	} else {
	    set navVisual [::loon::classes::NavigatorVisual new\
			       $navigator $navObj $visualid $canvas $map]	    
	}
	## The Visual object draws itself on creation
	

	set navChng [${modelns}::my navigator use $navigator systembind state add\
			 all "[self] navigatorconfigured %nav %e"]

	dict set navigators $navigator\
	    [dict create\
		 object $navObj\
		 visual $navVisual\
		 stateBinding $navChng\
		 objectns [info object namespace $navObj]]
		
	## So that it does not end up on top of the decorator elements
	catch {
	    uplevel #0 [list $canvas lower\
			    "layer&&${visualid}&&$navigator"\
			    "layer&&$visualid&&point"]
	    uplevel #0 [list $canvas raise\
			    "layer&&${visualid}&&(navigator||navigatorLabel||navigatorPathEnd)&&$navigator"\
			    "layer&&$visualid"]
	}
	
    }
    


    method updateNavigatorCoords {{which "all"}} {
	my variable modelns curX curY isCurXYinvalidated
	
	if {$which eq "all"} {
	    set which [dict keys $navigators] 
	}
	
	if {[llength $which] > 0 && $isCurXYinvalidated} {
	    my updateCurXY
	}

	foreach nav $which {

	    set navigatorns [dict get $navigators $nav objectns]
	    
	    set from [$navigatorns cget -from]
	    set to [$navigatorns cget -to]
	    set path [concat $from $to]

	    set pathCoords {}
	    set seenCoords {}
	    
	    switch -- [llength $path] {
		0 {
		    ## navigator has no place on graph
		    set xNav 50
		    set yNav 50		    
		}
		1 {
		    ## navigator is on node, no path
		    set ifrom [lsearch -exact [set $nodes_var] $path]

		    if {$ifrom eq -1} {
			## should not happen
			#puts "warning: -from node \"$path\" not found."
			continue
		    }
		    
		    set xNav [lindex $curX $ifrom]
		    set yNav [lindex $curY $ifrom]
		}
		default {
		    ## navigator has an associated path
		    set p [$navigatorns cget -proportion]

		    set nodes [set $nodes_var]
		    
		    foreach efrom $from {
			set ifrom [lsearch -exact $nodes $efrom]

			if {$ifrom eq -1} {
			    ## should not happen
			    #puts "warning: -from node \"$efrom\" not found."
			    return
			}
			
			lappend pathCoords [lindex $curX $ifrom]
			lappend pathCoords [lindex $curY $ifrom]
		    }

		    set seenCoords $pathCoords
		    
		    set xNav [lindex $curX $ifrom]
		    set yNav [lindex $curY $ifrom]

		    set hasFirstTo FALSE
		    foreach eto $to {
			set ito [lsearch -exact $nodes $eto]

			if {$ito eq -1} {
			    ## should not happen
			    # puts "warning: -to node \"$eto\" not found."
			    return
			}
			if {!$hasFirstTo} {
			    set xFirstTo [lindex $curX $ito]
			    set yFirstTo [lindex $curY $ito]
			    set hasFirstTo TRUE
			}
			
			lappend pathCoords [lindex $curX $ito]
			lappend pathCoords [lindex $curY $ito]
		    }
		    
		    if {$hasFirstTo && $p > 0} {
			set xNav [expr {$xNav + double($p) * ($xFirstTo - $xNav)}]
			set yNav [expr {$yNav + double($p) * ($yFirstTo - $yNav)}]
			lappend seenCoords $xNav $yNav
		    }
		}
	    }
	    
	    switch -- [llength $pathCoords] {
		0 {
		    set pathCoords [list $xNav $yNav $xNav $yNav]
		}
		2 {
		    lappend pathCoords {*}$pathCoords
		}
	    }
	    
	    switch -- [llength $seenCoords] {
		0 {
		    set seenCoords [list $xNav $yNav $xNav $yNav]
		}
		2 {
		    lappend seenCoords {*}$seenCoords
		}
	    }
	    
	    
	    #puts "AAA $xNav, $yNav, $pathCoords, $seenCoords"
	    [dict get $navigators $nav visual] updateCoords $xNav $yNav $pathCoords $seenCoords
	    
	    #puts "   coords: $t_x $t_y"
	}
	
    }
    ## Show Navigator Bindings
    
    method raiseSelectedToTop {} {
	my variable isVisible visualid canvas n_var
	
	next
      	if {$isVisible && [set $n_var] ne 0} {
	    uplevel #0 [list $canvas raise "navigatorPathEnd&&$visualid" $visualid]
	    uplevel #0 [list $canvas raise "(navigator||navigatorLabel)&&$visualid" $visualid]
	}
    }


    ##
    method setActiveNavigator {} {
	my variable canvas
	
	# puts "set active navigator to: [set $activeNavigator_var]"

	## highlight navigator
	uplevel #0 [list $canvas itemconfigure "model&&navigator"\
			-outline black -width 1]
	if {[llength [set $activeNavigator_var]] ne 0} { 
	    uplevel #0 [list $canvas itemconfigure\
			    "model&&navigator&&[set $activeNavigator_var]"\
			    -outline $::loon::Options(select-color)\
			    -width 4]
	}
	
	
    }
    
}

