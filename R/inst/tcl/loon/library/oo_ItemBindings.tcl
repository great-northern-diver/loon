
::oo::class create loon::classes::ItemBindings {   

    superclass loon::classes::Bindings
    
    variable canvas tagorid doCatch map
    
    
    constructor {idPrefix {DoCatch TRUE}} {
	
	set doCatch $DoCatch
	set canvas ""
	set map ""
	
	array set tagorid {}

	next $idPrefix
    }
 
    method InfoDebug {args} {
	next canvas tagorid doCatch {*}$args
    }
    

    method setCanvas {Canvas} {
	set canvas $Canvas
    }

    method setMap {Map} {
	set map $Map
    }
    
   
    method add {tagOrId {sequence ""} {command ""}} {
	
	if {$sequence eq ""} {
	    return [$canvas bind $tagOrId]
	} elseif {$command eq ""} {
	    return [$canvas bind $tagOrId $sequence]
	} else {
	    set id [next $sequence $command]
	    
	    set tagorid($id) $tagOrId
	    $canvas bind $tagOrId $sequence "+ [self] notify $id %x %y"
	    
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
	    set eventCode [$canvas bind $tagorid($i) $Events($i) $Code($i)]	
	    $canvas bind $tagorid($i) $Events($i)\
		[regsub -- "[self] notify $i %x %y\n" $eventCode ""]
	    
	    array unset tagorid $i
	} 

	next $id
    }

    
    method get {args} {

	if {[llength $args] eq 1} {
	    return [list $tagorid($args) {*}[next {*}$args]]
	} else {
	    return [next {*}$args]
	}	
    }

    
    
    method notify {id x y} {
	my variable Code substitutions
	
	set xy [$map mapS2D $x $y]
	
	set cmdeval [string map [concat $substitutions\
				     %b $id\
				     %O $canvas\
				     %x [lindex $xy 0]\
				     %y [lindex $xy 1]]\
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
		puts "\"$obj\" item binding \"$id\" caused the following error: $msg"
	    }
	} else {
	    uplevel #0 $cmdeval
	}
    }
}
