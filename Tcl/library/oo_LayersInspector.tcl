oo::class create loon::classes::LayersInspector {

    superclass ::loon::classes::Inspector2

    variable treeview entryvar\
	widgetLayerBinding visibleImg invisibleImg


    constructor {Path} {
	set widgetLayerBinding ""

	set visibleImg [image create bitmap\
			    -file [file join $::env(LOON_LIBRARY) images visible.xbm]]

	set invisibleImg [image create bitmap\
			      -file [file join $::env(LOON_LIBRARY) images invisible.xbm]]

	next $Path

    }

    destructor {
	my variable activewidget
	if {$widgetLayerBinding ne ""} {
	    catch {$activewidget layer systembind delete $widgetLayerBinding}
	    set widgetLayerBinding ""
	}
	next
    }



    ## Don't need State Binding but need Layer Binding
    method RegisterActivewidget {} {
	my variable activewidget
	if {$activewidget ne ""} {
	    set widgetLayerBinding\
		[uplevel #0 [list $activewidget systembind layer add all\
				 "[self namespace]::my ActivewidgetLayerEvents %e %l"]]
	}

	my repopulate

    }
    method UnregisterActivewidget {} {
	my variable activewidget
	if {$widgetLayerBinding ne ""} {
	    catch {uplevel #0 [list $activewidget systembind layer delete\
			    $widgetLayerBinding]}
	    set widgetLayerBinding ""
	}
    }

    method CheckNewActivewidget {widget} {
	if {![info object isa typeof $widget\
		  "::loon::classes::withLayers"]} {
	    error "$widget does not have layers"
	}
    }




    method InfoDebug {args} {
	next treeview entryvar widgetLayerBinding {*}$args
    }

    method getTreeview {} {
	return $treeview
    }

    method Make {} {
	my variable path

	frame $path -class LoonLayersInspector

	frame ${path}.stv
	set treeview ${path}.stv.treeview
	ttk::treeview $treeview -selectmode browse -columns {type id}
	$treeview heading #0 -text label -anchor c
	$treeview heading type -text type
	$treeview heading id -text id
	$treeview column type -width 60 -stretch FALSE
	$treeview column id -width 60 -stretch FALSE
	$treeview column #0 -width 100 -stretch TRUE


	scrollbar ${path}.stv.scroll -orient vertical -command "$treeview yview"
	$treeview configure -yscrollcommand "${path}.stv.scroll set"

	pack ${path}.stv -side top -fill both -expand 1 -padx {3 0} -pady {3 0}
	pack ${path}.stv.scroll -side right -fill y
	pack $treeview -side right -fill both -expand 1

	set control [frame ${path}.control]
	pack $control -side top -fill x

	foreach xbm {up down promote demote visible invisible plus minus scaleto}\
	    cmd {raise lower promote demote visible invisible addGroup expunge scaleto} {
		set img [image create bitmap\
			     -file [file join $::env(LOON_LIBRARY) images ${xbm}.xbm]]

		pack [button ${control}.$cmd -image $img\
			  -command [list [self namespace]::my $cmd]]\
		    -side left -padx 5
	    }


	set label [frame ${path}.label]
	set label_label [label ${label}.label -text "change label to:"]
	set entryvar ${label}.entry.var
	entry ${label}.entry -textvariable $entryvar -width 4

	set button_set\
	    [button ${label}.set -text "set"\
		 -command [list [self namespace]::my setLabel]]

	bind $label.entry <Key-Return> [list [self namespace]::my setLabel]
	bind $label.entry <KP_Enter> [list [self namespace]::my setLabel]

	pack $label -side top -fill x
	pack $label_label -side left -padx {3 0} -pady {3 0}
	pack $label.entry -side left -fill x -expand 1
	pack $label.set -side left -padx {2 0} -pady 1

	$treeview tag configure visible\
	    -foreground black
	#[option get $path visible_color ""]
	$treeview tag configure notvisible\
	    -foreground [option get $path notvisible_color ""]
	$treeview tag configure partlyvisible\
	    -foreground [option get $path partlyvisible_color ""]

	focus $treeview
    }



    method ActivewidgetLayerEvents {event layer} {
	#puts "layers inspector view update $ActiveWidget $event $layer"
	my variable activewidget

	if {$activewidget eq ""} {return}

	switch -- $event {
	    add {
		my AddLayer $layer
	    }
	    move {
		set parent [my Root2N [$activewidget layer getParent $layer]]
		set index [$activewidget layer getIndex $layer]
		$treeview move $layer $parent $index
	    }
	    delete {
		$treeview delete $layer
	    }
	    relabel {
		$treeview item [my Root2N $layer]\
		    -text [$activewidget layer getLabel $layer]
	    }
	}
	## always retag visiblility
	my retagVisibility
    }


    method scaleto {} {
	my variable activewidget
	if {$activewidget eq ""} {return}

	set node [$treeview selection]
	if {[catch {[$activewidget scaleto layer $node]}]} {
	    bell
	}
    }

    method TreeAction {what} {
	my variable activewidget
	if {$activewidget eq ""} {return}

	set node [$treeview selection]
	if {[catch {$activewidget layer $what $node} msg]} {
	    bell
	    puts "Warning: $msg"
	}
    }

    method raise {} {
	my TreeAction raise
    }

    method lower {} {
	my TreeAction lower
    }

    method promote {} {
	my TreeAction promote
    }

    method demote {} {
	my TreeAction demote
	$treeview item [$treeview parent [$treeview selection]] -open 1
    }

    method visible {} {
	my TreeAction show
    }

    method invisible {} {
	my TreeAction hide
    }

    method addGroup {} {
	my variable activewidget
	if {$activewidget eq ""} {return}
	set node [$treeview selection]
	set parent [$activewidget layer getParent $node]
	set index [$activewidget layer getIndex $node]
	incr index
	set id [$activewidget layer group -parent $parent -index $index]
	## switch current selection
	$treeview selection set $id
    }

    method expunge {} {

	my TreeAction expunge

#	my variable activewidget

#	if {$activewidget eq ""} {return}
#	set node [$treeview selection]
#	$activewidget layer expunge $node
    }

    method setLabel {} {
	my variable activewidget
	if {$activewidget eq ""} {return}

	set node [$treeview selection]

	set label [set ::$entryvar]
#	puts "label: $label"

	if {[regexp "^\\s*$" $label]} {
	    ::loon::warning "no valid label"
	} else {
	    set label [regsub "^\\s*" $label ""]
	    set label [regsub "\\s*$" $label ""]

	    $activewidget layer relabel $node $label
	    uplevel #0 [list set $entryvar ""]
	    focus $treeview
	}
    }

    method Erase {} {
	$treeview delete [$treeview children {}]
    }

    method repopulate {} {
	my variable activewidget
	my Erase
	if {$activewidget eq ""} {return}


	set layers [$activewidget layer getDescendants "root"]

	foreach layer $layers {
	    my AddLayer $layer
	}

	# select first element
	$treeview selection set [lindex [$treeview children {}] 0]
	my retagVisibility
    }

    method Root2N {node} {
	if {$node eq "root"} {
	    set node ""
	}
	return $node
    }

    method AddLayer {layer} {
	my variable activewidget
	if {$activewidget eq ""} {return}

	set parent [my Root2N [$activewidget layer getParent $layer]]
	set index [$activewidget layer getIndex $layer]
	set type [$activewidget layer getType $layer]
	set label [$activewidget layer getLabel $layer]

	#set label [$activewidget layer getLabel $layer]
	$treeview insert $parent $index -id $layer\
	    -text " $label"\
	    -values [list $type $layer]

    }

    method retagVisibility {} {
	my variable activewidget
	if {$activewidget eq ""} {return}

	set layers [$activewidget layer getDescendants root FALSE]

	#puts "retag visibility: $layers"

	foreach l $layers {
	    if {[$activewidget layer isVisible $l]} {
		$treeview item $l -image $visibleImg

		if {[$activewidget layer isGroup $l]} {
		    switch -- [$activewidget layer groupVisibility $l] {
			all {
			    $treeview item $l -tag "visible"
			}
			part {
			    $treeview item $l -tag "partlyvisible"
			}
			none {
			    $treeview item $l -tag "notvisible"
			}
			default {
			    error "layer view unexpected state"
			}
		    }
		} else {
		    if {[$activewidget layer layerVisibility $l]} {
			$treeview item $l -tag "visible"
		    } else {
			$treeview item $l -tag "notvisible"
		    }
		}
	    } else {
		$treeview item $l -image $invisibleImg -tag "notvisible"
	    }
	}
	return
    }



}
