::oo::class create loon::classes::CanvasBindings {   

    superclass loon::classes::Bindings
    
    variable canvas doCatch map
    
    constructor {idPrefix {DoCatch TRUE}} {
	
	set doCatch $DoCatch

	set canvas ""
	set map ""

	next $idPrefix
    }
 
    method InfoDebug {args} {
	next canvas doCatch {*}$args
    }
        
    method setCanvas {Canvas} {
	set canvas $Canvas
    }
    method setMap {Map} {
	set map $Map
    }
    
    
   
    method add {{sequence ""} {command ""}} {
	
	if {$command eq ""} {
	    return [bind $canvas $sequence]
	} else {
	    set id [next $sequence $command]
	    
	    bind $canvas $sequence "+[self] notify $id %x %y %w %h"
	    
	    return $id
	}
	
    }
    

    method delete {id} {
	# delete canvas binding	
	my variable Events Code Order

	if {$id eq "all"} {
	    set $ids [array names Order]
	} else {
	    set ids $id
	}
	
	foreach i $ids {
	    set eventCode [bind $canvas $Events($i)]	

	    bind $canvas $Events($i)\
		[regsub -- "\n?[self] notify $i %x %y %w %h" $eventCode ""]
	} 
	
	next $id
    }

    method notify {id x y w h} {
	my variable Code substitutions
	
	set xy [$map mapS2D $x $y]
	
	set cmdeval [string map [concat $substitutions\
				     %b $id\
				     %O $canvas\
				     %x [lindex $xy 0]\
				     %y [lindex $xy 1]\
				     %w $w %h $h]\
			 $Code($id)]
	
	if {$doCatch} {
	    if {[catch {uplevel #0 $cmdeval} msg]} {
		set i [lsearch -exact $substitutions "%W"]
		if {$i ne -1} {
		    incr i
		    set obj [lindex $substitutions $i]
		} else {
		    set obj $object
		}
		puts "\"$obj\" canvas binding \"$id\" caused the following error: $msg"
	    }
	} else {
	    uplevel #0 $cmdeval
	}
    }
}
