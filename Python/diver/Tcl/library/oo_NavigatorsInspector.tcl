
oo::class create loon::classes::NavigatorsInspector {

    superclass ::loon::classes::Inspector2

    variable treeviewN treeviewC

    
    constructor {Path} {
	
	next $Path navigator "%nav"
	
    }


    method NavigatorSelected {} {
	my variable activewidget
	
	my ReloadContexts
	## Change the active navigator and context inspector
    }
    

    method ContextSelected {} {
	
    }

    method ReloadNavigators {} {
	my variable activewidget
	$treeviewN delete [$treeview children ""]

	return
	if {$activewidget ne ""} {
	    foreach nav [$activewidget navigator ids] {
		$treeviewN insert "" end -id $nav\
		    -text [$activewidget navigator getLabel $nav]\
		    -values $nav
	    }
	}

    }
    
    method ReloadContexts {} {
	my variable activewidget
	
	$treeviewC delete [$treeviewC children ""]

	return
	if {$activewidget ne ""} {
	    ## which navigator is selected?
	    set nav [$treeviewN selection]
	    if {$nav ne ""} {
		foreach context [$activewidget navigator use $nav context list] { 
		    $treeviewC insert "" end -id $context\
			-text [$activewidget navigator use $nav context getLabel $context]\
			-values [list [$activewidget navigator use $nav context getType $context]\
				     $context]
		}
	    }
	}
    }
    


    method ActivewidgetEvents {event navigator} {
	my variable activewidget

	return
	
	switch -- $event {
	    add {
		$treeviewN insert "" end -id $navigator\
		    -text [$activewidget navigator getLabel $navigator]\
		    -values $navigator
	    }
	    delete {
		$treeviewN delete $navigator
	    } 
	    relabel {
		$treeviewN item $navigator -text [$activewidget navigator getLabel $navigator]
	    }
	}
	
#	puts $treeviewN
	set id [$treeviewN selection]
	if {$id eq "" && [llength [$treeviewN children ""]]} {
	    set navigator [lindex [$treeviewN children ""] 0]
	    $treeviewN selection set $navigator
	}
	
    }

    method RegisterActivewidget {} {
	my variable activewidget
	next

	return
	
	## repopulate
	if {$activewidget ne ""} {
	    set navigators [$activewidget navigator ids]
	    if {[llength $navigators] > 0} {
		$treeviewN selection set [lindex $navigators 0]
	    }
	} else {
	    set navigators ""
	}
    }
    
    method UnregisterActivewidget {} {
	my variable activewidget path
	next
	if {$activewidget ne ""} {
#	    uplevel #0 [list ${path}.${activeNavigatorInspector}.inspector\
#			    configure -activewidget "" -activenavigator ""]
	}
    }
    
    
    method CheckNewActivewidget {widget} {
	if {![info object isa typeof $widget\
		  "::loon::classes::withNavigators"]} {
	    error "$widget does not support navigators."
	}
    }

    method Make {} {
	my variable path
	
	frame $path -class NavigatorInspector
	
	
	## Navigatos
	pack [label ${path}.labNavigators -text "Navigators:"] -side top -anchor w

	pack [frame ${path}.navigatorselector] -fill x -side top
	
	

	set treeviewN [::ttk::treeview ${path}.navigatorselector.treeview\
			  -height 5 -selectmode browse\
			  -columns id]
	
	$treeviewN heading #0 -text label -anchor c
	$treeviewN heading id -text id
	$treeviewN column id -width 100 -stretch FALSE
	$treeviewN column #0 -width 100 -stretch TRUE

	
	set scroll ${path}.navigatorselector.scroll
	pack $treeviewN -side left -anchor n -fill x -expand 1 -padx {5 0} -pady 5
	pack [::tk::scrollbar $scroll\
		 -orient vertical -command "$treeviewN yview"]\
	    -side left -anchor n -fill y -padx {0 5} -pady 5
	
	
	$treeviewN configure -yscrollcommand "$scroll set"
		
	bind $treeviewN <<TreeviewSelect>> "[self namespace]::my NavigatorSelected"
	
	
	## Context Treeview
	pack [label ${path}.labContexts -text "Contexts:"] -side top -anchor w
	pack [frame ${path}.contextselector] -fill x -side top
	set treeviewC [::ttk::treeview ${path}.contextselector.treeview\
			  -height 5 -selectmode browse\
			  -columns {type id}]
	
	$treeviewC heading #0 -text label -anchor c
	$treeviewC heading type -text type
	$treeviewC heading id -text id
	$treeviewC column type -width 80 -stretch FALSE
	$treeviewC column id -width 70 -stretch FALSE
	$treeviewC column #0 -width 100 -stretch TRUE
	
	set scrollC ${path}.contextselector.scroll
	pack $treeviewC -side left -anchor n -fill x -expand 1 -padx {5 0} -pady 5
	pack [::tk::scrollbar $scrollC\
		 -orient vertical -command "$treeviewC yview"]\
	    -side left -anchor n -fill y -padx {0 5} -pady 5
	
	
	$treeviewC configure -yscrollcommand "$scrollC set"
		
	bind $treeviewC <<TreeviewSelect>> "[self namespace]::my ContextSelected"
	
	## Active Path
	pack [label ${path}.labActivePath -text "Active Path:"] -side top -anchor w
	pack [frame ${path}.activePath] -fill x -side top
       	
	set textW [::tk::text ${path}.activePath.text -height 5 -width 5]
	set scrollT ${path}.activePath.scroll
	pack $textW -side left -anchor n -fill x -expand 1 -padx {5 0} -pady 5
	pack [::tk::scrollbar $scrollT\
		 -orient vertical -command "$textW yview"]\
	    -side left -anchor n -fill y -padx {0 5} -pady 5
	
	$textW configure -yscrollcommand "$scrollT set"
	
	
    } 
    
    
    
    
}
