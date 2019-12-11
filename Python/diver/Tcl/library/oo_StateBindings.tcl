

::oo::class create loon::classes::StateBindings {   

    superclass loon::classes::Bindings
    
    variable doCatch inConfigurationVar hasInConfigurationVar

    constructor {idPrefix {DoCatch TRUE} {InConfigurationVar ""}} {
	set doCatch $DoCatch	
        set inConfigurationVar $InConfigurationVar

        if {$inConfigurationVar ne ""} {
            set hasInConfigurationVar TRUE
        } else {
            set hasInConfigurationVar FALSE
        }

       	next $idPrefix
    }
    
    method InfoDebug {args} {
	next doCatch {*}$args
    }
    
    method notify {object events} {
	
	my variable Events Code Order substitutions
	

        set listIdToNotify {}
        
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
                lappend listIdToNotify $id
            }            
        }
        
        set nToNotify [llength $listIdToNotify]
       
        if {$nToNotify > 0} {
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

            set lastId [lrepeat $nToNotify FALSE]
            lset lastId end TRUE
            
        } else {
            set lastId {}
        }

        
        
        foreach id $listIdToNotify isLastId $lastId {
            
            #puts "Substitutions: [list {*}$substitutions %e $substEvents %b $id %O $object]"
            
            if {$isLastId && $hasInConfigurationVar} {
                set $inConfigurationVar FALSE
            }

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




