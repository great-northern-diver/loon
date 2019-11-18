

oo::class create loon::classes::LayerBindings {
    
    superclass loon::classes::Bindings
    
    variable doCatch
    
    constructor {idPrefix {DoCatch TRUE}} {
	
	set doCatch $DoCatch
	
	next $idPrefix
    }

    method notify {object layer event} {
	
	my variable Events Code Order substitutions
	
	foreach id $Order {
	    set toNotify FALSE
	    if {"all" in $Events($id)} {
		set toNotify TRUE
	    } else {
		if {$event in $Events($id)} {
		    set toNotify TRUE
		}
	    }
	    
	    
	    if {$toNotify} {
		set cmdeval [string map [list {*}$substitutions\
					     %e $event\
					     %b $id\
					     %l $layer\
					     %O $object]\
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
			puts "\"$obj\" layer binding \"$id\" caused the following error: $msg"
		    }
		} else {
		    uplevel #0 $cmdeval
		}
	    }
	}
	
    }
}
