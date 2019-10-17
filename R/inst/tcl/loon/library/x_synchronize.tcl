
namespace eval loon {

    variable LinkingG2M;# Group to Model map
    variable LinkingM2G;# Model to Group map
    variable letters
    variable lgwidgets; # linkingGroupWidgets
    
    array set LinkingG2M {}
    array set LinkingM2G {}
    set lgwidgets {}
    
    set letters {A B C D E F G H I J K L M N O P Q R S T U V Q X Y Z}
    
    
    # add for add a distinct linking sub group
    proc getLinkingGroups {widget {add FALSE} {newGroup ""}} {
	variable letters
	variable LinkingG2M

	set groups "none"

	set currentLinkingGroup [${widget}::my cget -linkingGroup]
	
	if {$currentLinkingGroup ne "none"} {
	    set n [llength $LinkingG2M($currentLinkingGroup)]
	    lappend groups "$currentLinkingGroup  \[$n linked\]"
	}

	
	set linkingGroups [array names LinkingG2M]
	foreach linkingGroup $linkingGroups {
	    if {$linkingGroup ne $currentLinkingGroup && $linkingGroup ne "none"} {
		set n [llength $LinkingG2M($linkingGroup)]
		lappend groups "$linkingGroup  \[$n linked\]"
	    }
	}
	
	if {$add} {
	    if {$newGroup ni $linkingGroups} {
		lappend groups "$newGroup  \[0 linked\]"
	    } else {
		set notadded 1
		foreach l $letters {
		    if {"$newGroup-$l" ni $linkingGroups} {
			lappend groups "$newGroup-$l  \[0 linked\]"
			set notadded 0
			break
		    }
		}
		if {$notadded} {
		    puts "Warning: no new group added."
		}
	    }
	}

	return $groups
    }

    ## Synchronize all models
    proc synchronizeModels {{models "all"}} {
	
    }



    proc synchronizePull {model linkingGroup linkingKey initState} {
	## return a dict with an entry for each state.
	
		
	## keep track which tags are still needed for each state
	## only the tag index is needed
	## TODO: Only if it is important for speed
	#set n_model [${model}::my cget -n]
	#set seq [::loon::listfns::lseq 0 [expr {$n_model-1}]]
	#array set i_tagsLeft {}
	#foreach state $states {
	#    set i_tagsLeft($state) $seq 
	#}
	
	set n_model [llength $linkingKey]
	
	foreach member [::loon::linkedMembersForGroup $linkingGroup $model] {
	    
	    set sharedStates [::loon::getSharedStates $model $member]
	    set n_member [set ${member}::n]
	    
	    if {[llength $sharedStates] eq 0} {
		continue
	    }
	    
	    ## which elements are linked?
	    set d [::loon::getTagMap $model $member $linkingKey]
	    set i_tag_model [dict get $d i_tag_model]
	    set i_tag_member [dict get $d i_tag_member]
	    
	    if {[llength $i_tag_model] eq 0} {
		continue
	    }
	    
	    foreach state $sharedStates {
		foreach i $i_tag_model\
		    val [::loon::listfns::subset [${member}::my cget -$state] $i_tag_member] {
			dict update initState $state var {			
			    lset var $i $val
			}
		    }
	    }
	}
	
	return $initState
    }


    proc synchronizePush {model {states ""}} {
	variable LinkingM2G

	## states eq "" mean push all linked states

	set n_model [set ${model}::n]
	
	if {![::loon::hasModelLinkedMembers $model] || $n_model eq 0} {
	    return
	}
	
	
	set hasModelDefaultLinkingKey [set ${model}::isDefaultLinkingKey]
	
	set modelLM [set ${model}::linkingKey]
	
	foreach member [::loon::linkedMembers $model] {
		    
	    set statesToPush [::loon::getSharedStates $model $member $states]
	    set n_member [set ${member}::n]

	    if {[llength $statesToPush] eq 0 || $n_member eq 0} {
		continue
	    }	

	    #puts "-- Push $model > $member : $statesToPush"
	    #puts "     states shared: $statesToPush"
	    #puts "     n_model $n_model, n_member $n_member"
	    
	    set hasMemberDefaultLinkingKey [set ${member}::isDefaultLinkingKey]
	    

	    ## now copy each state
	    set args [list .nosync TRUE]

	    if {$hasModelDefaultLinkingKey &&\
		    $hasMemberDefaultLinkingKey} {
		## Cheapest
		if {$n_model eq $n_member} {
#		    puts "      n_model = n_member"
		    foreach state $statesToPush {
			lappend args -$state [set ${model}::$state]
		    }
		} elseif {$n_model > $n_member} {
		    ## Cheaper
#		    puts "      n_model > n_member"
		    set nm1 [expr {$n_member - 1}]
		    foreach state $statesToPush {
			lappend args -$state [lrange [set ${model}::$state] 0 $nm1] 
		    }
		} else {
		    ## Cheap
		    ## n_model < n_member
#		    puts "      n_model < n_member"
		    lappend args -which $modelLM
		    foreach state $statesToPush {
			lappend args -$state [set ${model}::$state] 
		    }
		}
	    } else {
		## Costly
#		puts "       costly sync push"
		set d [::loon::getTagMap $model $member $modelLM]
		set i_tag_model [dict get $d i_tag_model]
		if {[llength $i_tag_model] eq 0} {
		    continue
		}
		set i_tag_member [dict get $d i_tag_member]
		
		lappend args -which $i_tag_member
		foreach state $statesToPush {
		    lappend args -$state [::loon::listfns::subset\
					      [set ${model}::$state]\
					      $i_tag_model]
		}
	    }
	    
	    
	    ## push changes
	    ${member}::my configure {*}$args
	    
	    # puts "$model pushed $statesToPush to $member"
#           puts "i_tag_model:  $i_tag_model"
#	    puts "i_tag_member: $i_tag_member"
#	    puts "arg $args"
	    
	}
    }
    

    proc getSharedStates {model member {modelStatesOfInsterest ""}} {
	## does the linked member share any linked states?
	
	if {$modelStatesOfInsterest eq ""} {
	    set modelStatesOfInsterest [${model}::my getLinkedStates]
	}
	
	set memberLinkedStates [${member}::my getLinkedStates]
	
	set sharedStates {}
	foreach state $modelStatesOfInsterest {
	    if {$state in $memberLinkedStates} {
		lappend sharedStates $state
	    }
	}
	
	return $sharedStates
    }


    ## need model linkingKey for pull
    proc getTagMap {model member modelLM} {
	## Does the linked member share any linking tags, i.e.
	## elements in the linkingKey
	
	set n_model [llength $modelLM]
	
	set memberLM [${member}::my cget -linkingKey]
	set n_member [${member}::my cget -n]
	
	set i_tag_model {}
	set i_tag_member {}
	
	if {$n_member > $n_model} {
	    set i 0
	    foreach tag $modelLM {
		set j [lsearch -exact $memberLM $tag]
		if {$j ne -1} {
		    lappend i_tag_model $i
		    lappend i_tag_member $j
		}		    
		incr i
	    }
	} else {
	    set i 0
	    foreach tag $memberLM {
		set j [lsearch -exact $modelLM $tag]
		if {$j ne -1} {
		    lappend i_tag_model $j
		    lappend i_tag_member $i
		}		    
		incr i
	    }		
	}
	
	return [dict create i_tag_model $i_tag_model i_tag_member $i_tag_member]
    }


    proc linkedMembers {model} {
	variable LinkingM2G
	variable LinkingG2M
	
       
	if {[catch {set group $LinkingM2G($model)}]} {
	    error "model $model is not registerd."
	}

	if {$group eq "none"} {
	    return ""
	}

       	set members $LinkingG2M($group)
	
	set i [lsearch -exact $members $model]
	if {$i eq -1} {
	    error "model $model not found in linkingGroup list."
	} else {
	    set members [lreplace $members $i $i]
	}

	return $members
    }

    proc hasGroupLinkedMembers {group {model ""}} {
	variable LinkingG2M
	
	if {$group eq "none"} {
	    return FALSE
	}
	
	set hasLinkedMembers FALSE

	if {[info exists LinkingG2M($group)]} {
	    set n [llength $LinkingG2M($group)]
	    if {$model in $LinkingG2M($group)} {
		incr n -1
	    }
	    if {$n > 0} {
		set hasLinkedMembers TRUE
	    }
	}	
	return $hasLinkedMembers	
    }

    proc linkedMembersForGroup {group {model ""}} {
	variable LinkingG2M
	
	if {$group eq "none"} {
	    return FALSE
	}
	
	set linkedMembers {}
	
	if {[info exists LinkingG2M($group)]} {

	    set linkedMembers $LinkingG2M($group)
	    
	    set i [lsearch -exact $linkedMembers $model]
	    if {$i ne -1} {
		set linkedMembers [lreplace $linkedMembers $i $i]
	    }
	}
	
	return $linkedMembers
    }

    proc hasModelLinkedMembers {model} {
	variable LinkingG2M
	
	set group [uplevel #0 [list ${model}::my cget -linkingGroup]]
	
	return [::loon::hasGroupLinkedMembers $group $model]
    }


    
    proc changeLinkingGroup {model} {
	variable LinkingG2M
	variable LinkingM2G

	set group [uplevel #0 [list ${model}::my cget -linkingGroup]]
	
	unmapModelFromGroup $model
	lappend LinkingG2M($group) $model

	set LinkingM2G($model) $group
	::loon::updateLinkingGroupWidgets
    }

    
    proc unmapModelFromGroup {model} {
	variable LinkingG2M	
	variable LinkingM2G
	
	set prevLG $LinkingM2G($model)
	
	set i [lsearch -exact $LinkingG2M($prevLG) $model]
	if {$i ne "-1"}  {
	    set LinkingG2M($prevLG) [lreplace $LinkingG2M($prevLG) $i $i]
	    
	    if {[llength $LinkingG2M($prevLG)] eq 0} {
		array unset LinkingG2M $prevLG
	    }
	} else {
	    error "could not remove $model from previous linkingGroup -$prevLG."
	}	
    }

    
    proc registerForLinking {model group} {
	variable LinkingG2M
	variable LinkingM2G
	
	if {$model in [array names LinkingM2G]} {
	    error "model is already registered for linking"
	}
		
	set LinkingM2G($model) $group
	lappend LinkingG2M($group) $model
	
	::loon::updateLinkingGroupWidgets
    } 

    
    
    
    ## only if model gets destroyed
    proc removeFromLinking {model} {
	variable LinkingM2G
	
	if {$model ni [array names LinkingM2G]} {
	    error "model was not registered for linking"
	}
	
	unmapModelFromGroup $model
	array unset LinkingM2G $model

	::loon::updateLinkingGroupWidgets
    }

    
    ## number of linked models excluding the specified model
    proc numberOfLinkedModels {model} {
	variable LinkingG2M
	
	set group [uplevel #0 [list ${model}::my cget -linkingGroup]]
	set linkedModels $LinkingG2M($group)
	
	set i [lsearch -exact $linkedModels $model]
	if {$i eq "-1"} {
	    ## TODO remove
	    error "internal data inconsitency for linking 2."
	}
	set n [llength $linkedModels]
	return [expr {$n - 1}]
    }


    proc updateLinkingGroupWidgets {} {
	variable LinkingG2M
	variable LinkingM2G
	variable lgwidgets
	
	foreach wns $lgwidgets {
	    
	    set widgetns [set ${wns}::activewidgetns]
	    if {$widgetns eq ""} {
		continue
	    }
	    if {![info exists LinkingM2G($widgetns)]} {
		## TODO: if widget got destroyed
		continue 
	    }
	    set lg $LinkingM2G($widgetns)
	    set nlinked [llength $LinkingG2M($lg)]
	    
	    if {$lg eq "none"} {
		set label none
	    } else {
		set label "$lg  \[$nlinked linked\]"
	    }
	    uplevel #0 [list set ${wns}::textvariable $label]
	}
    }

}

    
    
 


