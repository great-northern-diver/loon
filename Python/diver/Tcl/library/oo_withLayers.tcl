
::oo::class create ::loon::classes::withLayers {

    superclass ::loon::classes::Bindable

    variable tree idPrefix nextIdNr\
	userLayerBindings systemLayerBindings

    constructor {} {

	set idPrefix layer
	set nextIdNr 0

	set userLayerBindings [::loon::classes::LayerBindings new\
				   layerBinding TRUE]
	set systemLayerBindings [::loon::classes::LayerBindings new\
				     layerSystembinding FALSE]


	## Tree Structure
	## dict with nested dicts
	## depth one visualid
	## depth two parent children type object
	set tree [dict create root\
		      [dict create\
			   parent "" children ""\
			   object "" type "group"\
			   visible TRUE label root]]

	next

    }

    method InfoDebug {args} {
	next userLayerBindings systemLayerBindings {*}$args
    }

    method layer {subcommand args} {

	switch -- $subcommand {
	    group -
	    polygon -
	    polygons -
	    rectangle -
	    rectangles -
	    oval -
	    text -
	    texts -
	    line -
	    lines -
	    points -
	    tkcanvas {
		return [my NewLayer $subcommand {*}$args]
	    }

	    add {
		return [my NewLayer {*}$args]
	    }

	    list {
		puts "layer list is deprecated, use layer ids"
		return [dict keys $tree]
	    }
	    ids {
		return [dict keys $tree]
	    }

	    getParent -
	    getSiblings -
	    getAncestors -
	    getChildren -
	    getDescendants -
	    getIndex -
	    getType -
	    getLabel -
	    isVisible -
	    layerVisibility -
	    groupVisibility -
	    hide -
	    show -
	    relabel {
		## is visible is the visible slot for the layer
		## layer- and group-visibility also checks if the ancestors are visible
		my ENE [lindex $args 0]
		return [my [string toupper $subcommand 0] {*}$args]
	    }

	    getObject {
		set layer [lindex $args 0]
		my ENE $layer
		return [my GetLayerObject $layer]
	    }

	    bbox {
		if {[llength $args] eq 0} {
		    set layer root
		} else {
		    set layer [lindex $args 0]
		    my ENE $layer
		}
		return [my BboxLayer $layer]
	    }

	    with -
	    use -
	    target {
		set layer [lindex $args 0]
		my ENE $layer
		if {$layer eq "root"} {
		    error "can not work with root layer."
		}
		return [[dict get $tree $layer object] {*}[lrange $args 1 end]]
	    }

	    move {
		my MoveLayer {*}$args
	    }

	    delete {
		## can delete multiple layers
		my DeleteLayer $args
	    }
	    expunge {
		## call delete
		my ExpungeLayer $args
	    }

	    promote -
	    demote -
	    raise -
	    lower {
		## convenience functions that can also be done with
		## move
		set layer [lindex $args 0]
		my ENE $layer
		my [string toupper $subcommand 0]Layer {*}$args
	    }


	    printTree {
		if {[llength $args] eq 0} {
		    my PrintTree
		} else {
		    my PrintTree $args
		}
	    }
	    isGroup {
		my ENE $args
		return [my IsGroup $args]
	    }


	    default {
		## Try to create a Layer
		puts "layer switch default"
		error "\"$subcommand\" is invalid"
		my NewLayer $subcommand {*}$args
	    }

	}

    }


    ## Bindings
    method BindLayer {systemOrUser cmd args} {

	switch -- $cmd {
	    ids {
		[set ${systemOrUser}LayerBindings] get
	    }
	    delete -
	    reorder -
	    get -
	    add {
		[set ${systemOrUser}LayerBindings] $cmd {*}$args
	    }
	    notify {
		error "notify bindings are not possible."
	    }
	    default {
		puts "bind $cmd, use add, [info level 0] & [info level -1]"
		[set ${systemOrUser}LayerBindings] add $cmd {*}$args
	    }
	}

    }

    method NotifyLayerBindings {layer events} {
	$userLayerBindings notify [self] $layer $events
	$systemLayerBindings notify [self] $layer $events
    }


    method SetSubstitutions {subst} {
	$userLayerBindings setSubstitutions $subst
	$systemLayerBindings setSubstitutions $subst
	next $subst
    }

    method AddSubstitutions {subst} {
	$userLayerBindings addSubstitutions $subst
	$systemLayerBindings addSubstitutions $subst
	next $subst
    }

    ## Tree Stuff

    method GetLayerObject {layer} {
	return [dict get $tree $layer object]
    }

    method Relabel {layer label} {
	if {$layer eq "root"} {
	    error "cannot relabel root layer"
	}
	if {[my GetLabel $layer] ne $label} {
	    dict set tree $layer label $label
	    my NotifyLayerBindings $layer relabel
	}
    }

    method GetLabel {layer} {
	return [dict get $tree $layer label]
    }

    method Hide {layer} {
	if {$layer eq "root"} {
	    error "cannot hide root layer"
	}
	if {[my IsVisible $layer]} {
	    dict set tree $layer visible FALSE
	    my NotifyLayerBindings $layer hide
	}

	return 0
    }
    method Show {layer} {
	if {![my IsVisible $layer]} {
	    dict set tree $layer visible TRUE
	    my NotifyLayerBindings $layer show
	}

	return 0
    }

    method IsVisible {layer} {
	return [dict get $tree $layer visible]
    }

    ## this is different from IsVisible
    ## as some ancestor group may be not visible
    method LayerVisibility {layer} {
	if {$layer eq "root"} {
	    return TRUE
	} elseif {![my IsVisible $layer]} {
	    return FALSE
	} else {
	    return [my LayerVisibility [my GetParent $layer]]
	}
    }


    method GroupVisibility {layer} {

	set layers [my GetDescendants $layer TRUE]

	if {![my IsGroup $layer]} {
	    error "layer \"$layer\" is not a group."
	}

	if {![my LayerVisibility $layer]} {
	    return "none"
	}

	set shown 0
	set hidden 0

	foreach l [my GetDescendants $layer FALSE] {
	    if {[my LayerVisibility $l]} {
		incr shown
	    } else {
		incr hidden
	    }
	}

	## all/part/none
	if {$shown eq "0"} {
	    return "none"
	} elseif {$hidden eq "0"} {
	    return "all"
	} else {
	    return "part"
	}
    }

    method GetType {layer} {
	return [dict get $tree $layer type]
    }

    method GetParent {layer} {
	return [dict get $tree $layer parent]
    }

    method GetSiblings {layer} {
	set parent [my GetParent $layer]
	set children [dict get $tree $parent children]

	set i [lsearch -exact $children $layer]
	if {$i eq "-1"} {
	    error "tree inconsistencies"
	}
	## Tcl takes of out of bound indexes care (empty string)
	return [list [lindex $children [expr {$i-1}]] [lindex $children [expr {$i+1}]]]
    }

    method GetAncestors {layer} {

	if {$layer eq "root"} {
	    error "root has not ancestors."
	}

	set parent [my GetParent $layer]

	if {$parent eq "root"} {
	    return "root"
	} else {
	    return [concat $parent [my GetAncestors $parent]]
	}
    }

    method GetChildren {layer} {
	return [dict get $tree $layer children]
    }

    method GetDescendants {{layer root} {include FALSE}} {

	if {$include} {
	    set out $layer
	} else {
	    set out {}
	}

	if {[dict get $tree $layer type] eq "group"} {
	    foreach child [dict get $tree $layer children] {
		lappend out {*}[my GetDescendants $child TRUE]
	    }
	}

	return $out
    }

    method GetIndex {layer} {
	set parent [my GetParent $layer]
	set children [my GetChildren $parent]

	set i [lsearch -exact $children $layer]
	if {$i eq "-1"} {
	    error "tree inconsistencies for layer $layer"
	}

	return $i
    }


    method NewLayer {type args} {

	if {[catch {
	    set layerObject\
		[::loon::classes::[string toupper $type 0]Layer\
		     new [self namespace]]
	}]} {
	    error "layer type \"$type\" does not exist"
	}

	set id "${idPrefix}$nextIdNr"

	## Extract Parent, Label, Index
	if {"-parent" in $args} {
	    set parent [::loon::listfns::extractAndRemoveOption\
			    args -parent ""]
	    my ENE $parent
	    if {![my IsGroup $parent]} {
		error "parent \"$parent\" is not a group"
	    }
	} else {
	    set parent "root"
	}

	if {"-index" in $args} {
	    set index [my ProcessLayerIndex\
			   [::loon::listfns::extractAndRemoveOption\
				args -index ""] $parent]
	} else {
	    set index 0
	}

	if {"-label" in $args} {
	    set layerlabel [::loon::listfns::extractAndRemoveOption\
				args -label ""]
	} else {
	    set layerlabel $id
	}


	if {[llength $args] > 0} {
	    $layerObject configure {*}$args
	}

	incr nextIdNr

	my AddLayer $id $type $layerObject $parent $index $layerlabel

	return $id
    }

    method AddLayer {layer type object {parent root} {index 0} {label ""}} {

    my ENE $parent

	set children [dict get $tree $parent children]

	set index [my ProcessLayerIndex $index $parent]

	dict set tree $parent children [linsert $children $index $layer]

	dict set tree $layer\
	    [dict create\
		 parent $parent object $object\
		 type $type children ""\
		 visible TRUE label $label]

	my NotifyLayerBindings $layer add
    }

    method MoveLayer {layer parent {index 0}} {
	my ENE $layer
	my ENE $parent

	if {$layer eq "root"} {
	    error "can not move root layer"
	}

	if {![my IsGroup $parent]} {
	    error "parent \"$parent\" is not a group"
	}

	set index [my ProcessLayerIndex $index $parent]

	if {$parent in [my GetDescendants $layer]} {
	    error "parent $parent is a descendant of layer $layer."
	}

	set currentParent [my GetParent $layer]
	set currentParentChildren [my GetChildren $currentParent]
	set i [my GetIndex $layer]

	if {$currentParent ne $parent || $index ne $i} {

	    dict set tree $currentParent children\
		[lreplace $currentParentChildren $i $i]

	    set children [my GetChildren $parent]

	    dict set tree $parent children [linsert $children $index $layer]
	    dict set tree $layer parent $parent

	    my NotifyLayerBindings $layer move
	}

	return 0
    }

    method ProcessLayerIndex {index parent} {
	if {$index eq "end"} {
	    set children [my GetChildren $parent]
	    set index [llength $children]
	} elseif {![string is integer $index]} {
	    error "index \"$index\" is not an integer"
	}
	return $index
    }

    method IsGroup {layer} {
	if {[dict get $tree $layer type] eq "group"} {
	    return TRUE
	} else {
	    return FALSE
	}
    }

    method PrintTree {{layer "root"} {indent ""}} {
	if {[my IsGroup $layer]} {
	    foreach child [my GetChildren $layer] {
		if {[my IsGroup $child]} {
		    puts "$indent +$child"
		    my PrintTree $child "$indent  "
		} else {
		    puts "$indent $child"
		}
	    }
	} else {
	    puts "$layer is not a group"
	}
    }


    method PromoteLayer {layer {position "before"}} {

	if {$layer eq "root"} {
	    error "root can not be promoted"
	}

	set parent [my GetParent $layer]
	if {$parent eq "root"} {
	    error "layer \"$layer\" can not be promoted any further."
	}

	set index [my GetIndex $parent]

	switch -- $position {
	    before {

	    }
	    after {
		incr index
	    }
	    default {
		error "position is \"$position\" but\
                       must be either \"before\" or \"after\"."
	    }
	}

	set grandparent [my GetParent $parent]
	my MoveLayer $layer $grandparent $index
	return 0
    }

    method DemoteLayer {layer {index 0}} {
	if {$layer eq "root"} {
	    error "root can not be demoted"
	}

	set next [lindex [my GetSiblings $layer] 1]
	if {$next eq "" || ![my IsGroup $next]} {
	    error "the right sibling of $layer is not a group."
	}

	my MoveLayer $layer $next $index
	return 0
    }

    method RaiseLayer {layer} {
	if {$layer eq "root"} {
	    error "root can not be raised"
	}

	set index [my GetIndex $layer]
	set parent [my GetParent $layer]


	if {$index eq 0} {
	    ## can not raise
	    error "layer \"$layer\" can not be raised any further."
	} else {
	    incr index -1
	    my MoveLayer $layer $parent $index
	}
	return 0
    }

    method LowerLayer {layer} {
	if {$layer eq "root"} {
	    error "root can not be lowered"
	}

	set index [my GetIndex $layer]
	set parent [my GetParent $layer]
	set nchildren [llength [my GetChildren $parent]]

	if {$index eq [expr {$nchildren - 1}]} {
	    ## can not lower
	    error "layer \"$layer\" can not be lowered any further."
	} else {
	    incr index
	    my MoveLayer $layer $parent $index
	}
	return 0
    }

    method DeleteLayer {layer} {
	my ENE $layer
	if {$layer eq "root"} {
	    error "root layer can not be deleted."
	}

	if {$layer eq "model"} {
	    error "model layer can not be deleted."
	}

	set parent [my GetParent $layer]
	set i [my GetIndex $layer]
	if {[my IsGroup $layer]} {
	    ## Move into parent layer
	    foreach child [my GetChildren $layer] {
		my MoveLayer $child $parent $i
		incr i
	    }
	}
	set i [my GetIndex $layer]
	dict unset tree $layer
	set children [my GetChildren $parent]
	dict set tree $parent children\
	    [lreplace $children $i $i]

	my NotifyLayerBindings $layer delete

	return 0
    }

    method ExpungeLayer {layer} {
	my ENE $layer

	set descendants [my GetDescendants $layer TRUE]
	if {"model" in $descendants} {
	    error "\"model\" layer is a descendant of layer \"$layer\" and can not be deleted."
	}

	foreach l [lreverse $descendants] {
	    my DeleteLayer $l
	}

	return 0
    }

    method BboxLayer {layer} {

	set layers [my GetDescendants $layer TRUE]

	set bminX ""
	set bminY ""
	set bmaxX ""
	set bmaxY ""

	foreach layer $layers {
	    if {![my IsGroup $layer]} {

		set obj [dict get $tree $layer object]

		set lminX [$obj getMinX]
		set lminY [$obj getMinY]
		set lmaxX [$obj getMaxX]
		set lmaxY [$obj getMaxY]


		if {$lminX ne "" && $lminY ne "" && $lmaxX ne "" && $lmaxY ne ""} {
		    if {$bminX eq ""} {
			set bminX $lminX
			set bmaxX $lmaxX
			set bminY $lminY
			set bmaxY $lmaxY
		    } else {
			if {$lminX < $bminX} {set bminX $lminX}
			if {$lmaxX > $bmaxX} {set bmaxX $lmaxX}
			if {$lminY < $bminY} {set bminY $lminY}
			if {$lmaxY > $bmaxY} {set bmaxY $lmaxY}
		    }
		}
	    }
	}

	return [list $bminX $bminY $bmaxX $bmaxY]
    }

    ## error if not exists
    method ENE {layer} {

	if {![dict exists $tree $layer]} {
	    error "layer \"$layer\" does not exist"
	}

    }

    method scaleto {what args} {
	my variable x y xTemp yTemp selected active



	switch -- $what {
	    plot {
		set bbox [my layer bbox model]
	    }
	    world {
		set bbox [my layer bbox root]
	    }
	    selected -
	    active {
		set which [set $what]

		if {![::loon::listfns::any $which]} {
		    puts "Warning: no points $what"
		    return
		}

		if {[llength $xTemp] eq 0} {
                    set tmp_x $x
                } else {
                    set tmp_x $xTemp
                }

		if {[llength $yTemp] eq 0} {
                    set tmp_y $y
                } else {
                    set tmp_y $yTemp
                }

		set mmx [::loon::listfns::MinMax\
			     [::loon::listfns::subsetLogical $tmp_x $which]]

		set mmy [::loon::listfns::MinMax\
			     [::loon::listfns::subsetLogical $tmp_y $which]]

		set bbox [list [lindex $mmx 0]\
			      [lindex $mmy 0]\
			      [lindex $mmx 1]\
			      [lindex $mmy 1]]

	    }
	    layer {
		set bbox [my layer bbox [lindex $args 0]]
	    }
	    default {
		error "scaleto \"$what\" is not valid. Need scalto:\
                       plot, selected, active, world and layer."
	    }
	}
	## If no data
	if {[lindex $bbox 0] eq ""} {
	    set bbox {0 0 1 1}
	}

	my ApplyScaltoChanges $what $bbox

	return 0
    }

    method ApplyScaltoChanges {what bbox {xyRangeFactor 1.2}} {

	my variable deltaX deltaY

	set seeXdir [expr {[lindex $bbox 2] - [lindex $bbox 0]}]
	set seeYdir [expr {[lindex $bbox 3] - [lindex $bbox 1]}]

	if {$seeXdir < 0.00001} {
	    set seeXdir 0.00001
	    lset bbox 0 [expr {[lindex $bbox 0] - 0.000005 }]
	}
	if {$seeYdir < 0.00001} {
	    set seeYdir 0.00001
	    lset bbox 1 [expr {[lindex $bbox 1] - 0.000005 }]
	}

	set tzoomX [expr {$deltaX/double($xyRangeFactor*$seeXdir)}]
	set tzoomY [expr {$deltaY/double($xyRangeFactor*$seeYdir)}]

	set dxyRangeFactor [expr {($xyRangeFactor-1)/2.0}]

	set tpanX [expr {[lindex $bbox 0] - $dxyRangeFactor*$seeXdir}]
	set tpanY [expr {[lindex $bbox 1] - $dxyRangeFactor*$seeYdir}]

	my configure -zoomX $tzoomX -zoomY $tzoomY -panX $tpanX -panY $tpanY

	return 0
    }


}
