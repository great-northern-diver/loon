
oo::class create loon::classes::CollectionBindings {
    
    superclass loon::classes::Bindings
    
    variable doCatch type typeSubst

    constructor {Type idPrefix DoCatch TypeSubst} {
	set type $Type

	## for example Glyph > %g
	if {$TypeSubst eq ""} { 
	    set typeSubst [format "\%%s" [string tolower [string range $Type 0 0]]]
	} else {
	    if {[string range $TypeSubst 0 0] ne "%"} {
		error "invalid substitution \"$TypeSubst\".\
                       A substitution string must start with %."
	    }
	    set typeSubst $TypeSubst
	}
	
	set doCatch $DoCatch
	next $idPrefix
    }

    method notify {object objectId events} {
	
	my variable Events Code Order substitutions
	
	foreach id $Order {
	    set toNotify FALSE
	    if {"all" in $Events($id)} {
		set toNotify TRUE
	    } else {
		foreach e $events {
		    if {$event in $Events($id)} {
			set toNotify TRUE
			continue
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
		
		set cmdeval [string map [list {*}$substitutions\
					     %e $substEvents\
					     %b $id\
					     %O $object\
					     $typeSubst $objectId]\
				 $Code($id)]
		
		## if {$doCatch} {}
		if {FALSE} {
		    if {[catch { uplevel #0 $cmdeval } msg]} {
			set i [lsearch -exact $substitutions "%W"]
			if {$i ne -1} {
			    incr i
			    set obj [lindex $substitutions $i]
			} else {
			    set obj $object
			}
			error "\"$obj\" $type binding \"$id\" caused the following error: $msg"
		    }
		} else {
		    uplevel #0 $cmdeval
		}
	    }
	}
    }
}
