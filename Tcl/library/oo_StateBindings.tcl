

::oo::class create loon::classes::StateBindings {   

    superclass loon::classes::Bindings
    
    variable doCatch

    constructor {idPrefix {DoCatch TRUE}} {
	set doCatch $DoCatch
	
	next $idPrefix
    }
    
    method InfoDebug {args} {
	next doCatch {*}$args
    }
    
    method notify {object events} {
	
	my variable Events Code Order substitutions
	
	foreach id $Order {
	    set toNotify FALSE
	    if {"all" in $Events($id)} {
		set toNotify TRUE
	    } else {
		foreach e $events {
		    set idevents $Events($id)
		    if {$e in $idevents} {
			set toNotify TRUE
			break
		    }
		}
	    }
	    if {$toNotify} {
		switch -- [llength $events] {
		    0 {
			set substEvents "{}"
		    }
		    1 {
			set substEvents "$events"
		    }
		    default {
			set substEvents "{$events}"
		    }
		} 

		#puts "Substitutions: [list {*}$substitutions %e $substEvents %b $id %O $object]"
		
		set cmdeval [string map [list {*}$substitutions %e $substEvents %b $id %O $object]\
				 $Code($id)]
		
		if {$doCatch} {
		    if {[catch { uplevel #0 $cmdeval } msg]} {
			set i [lsearch -exact $substitutions "%W"]
			if {$i ne -1} {
			    incr i
			    set obj [lindex $substitutions $i]
			} else {
			    set obj $object
			}
			error "\"$obj\" state binding \"$id\" caused the following error: $msg"
		    }
		} else {
		    uplevel #0 $cmdeval
		}
	    }
	}
    }
}




