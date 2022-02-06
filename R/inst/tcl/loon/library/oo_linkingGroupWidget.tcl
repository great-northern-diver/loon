

namespace eval loon {
    
    proc linkingGroupWidget {path args} {
	set o [::loon::classes::linkinGroupWidget new $path]
	
	if {[llength $args] > 0} {
	    $o configure {*}$args
	}
	
	uplevel #0 rename $path $path.fr
	uplevel #0 rename $o $path
	
	return $path
    }
    
}

::oo::class create loon::classes::linkinGroupWidget {

    superclass ::loon::classes::Configurable
    
    variable path activewidget activewidgetns textvariable combobox

    constructor {Path} {
	my variable configurableOptions

	set path $Path
	set activewidget ""
	set textvariable "none"
	set activewidgetns ""
	next
	
	my New_state linkingGroup string 1 none
	lappend configurableOptions activewidget
	
	my Make
	
	lappend ::loon::lgwidgets [self namespace]
	
	bind $path <Destroy> "[self namespace]::my destroy"

	my SetStateDescription linkingGroup\
	    "linking group to display"
	
    }
    
    destructor {
	set i [lsearch -exact $::loon::lgwidgets [self namespace]]
	if {$i ne "-1"} {
	    set ::loon::lgwidgets [lreplace $::loon::lgwidgets $i $i]
	}       
    }
    
    method Make {} {
	
	frame $path -class LoonLinkingGroupWidget
	
	set combobox [::ttk::combobox ${path}.combobox\
			  -textvariable [my varname textvariable]\
			  -values {A B C}]
	pack $combobox -fill x -expand TRUE
	
	bind $combobox <<ComboboxSelected>> "[self namespace]::my ChangeLinkingGroup select"

	foreach ev {<Return> <KP_Enter>} {
	    bind $combobox $ev  "[self namespace]::my ChangeLinkingGroup enter"
	}
	$combobox configure -postcommand "[self namespace]::my ShowLinkingGroups"
	
    }
    
    method ShowLinkingGroups {} {
	if {$activewidget eq ""} {
	    set groups ""
	} else {
	    set groups [::loon::getLinkingGroups\
			    $activewidgetns TRUE [set ${activewidgetns}::n]]
	}
	
	$combobox configure -values $groups
    }
    
    method ChangeLinkingGroup {how} {
	set lg [$combobox get]
	set lg [regsub "\\s\\s\\\[\\d+ linked\\\]$" $lg ""]
	set lg [regsub "^\\s*" $lg ""]
	set lg [regsub "\\s*$" $lg ""]
	if {$lg eq ""} {
	    puts "Warning: can not have empty string for -linkingGroup"
	    bell
	    return
	}
	
	if {$activewidget ne ""} {
	    if {[::loon::hasGroupLinkedMembers $lg $activewidgetns]} {
		set sync [string tolower\
			      [::loon::loon_dialog\
				   -parent [winfo toplevel $activewidget]\
				   -title "Linked Plots"\
				   -main "Synchronize $activewidget?"\
				   -sub "Some other plot widgets have\n\
                                    the linkingGroup \"$lg\".\n\
                                    You either need to push or pull the\n\
                                    linked states of the $activewidget."\
				   -bitmap questhead\
				   -options {Cancel Push Pull}]]
		if {$sync ni {push pull}} {
		    ## fix changed entry
		    ::loon::updateLinkingGroupWidgets
		} else {
		    uplevel #0 [list $activewidget configure\
				    -linkingGroup $lg -sync $sync]
		}
	    } else {
		#puts "don't ask sync"
		uplevel #0 [list $activewidget configure -linkingGroup $lg]
	    } 
	}
    }
    
    
     method HookAfterStatesSet {} {
	my variable changedStates 
	if {"activewidget" in $changedStates} {
	    if {$activewidget eq ""} {
		set textvariable "none"
	    } else {
		::loon::updateLinkingGroupWidgets
	    }
	}

	next
    }
    
    
     method EvalConfigure {} {
	my variable confDict
	
	next 
	
	if {[dict get $confDict has_activewidget]} {
	    set arg_widget [dict get $confDict arg_activewidget]
	    if {$arg_widget ne ""} {
		
		set w $arg_widget
		
		if {![info object isa typeof $w\
			  "::loon::classes::Linkable"]} {
		    error "$arg_widget is not Linkable"
		}
		set ns [info object namespace $w]
	    } else {
		set ns ""
	    }
	    dict set confDict new_activewidget $arg_widget
	    dict set confDict new_activewidgetns $ns
	}
     }
    
    
       
    

    
}
