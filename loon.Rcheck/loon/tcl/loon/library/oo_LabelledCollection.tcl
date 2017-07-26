
oo::class create loon::classes::LabelledCollection {

    superclass loon::classes::fancyInfo
    
    variable userBindings systemBindings type objects idPrefix nextIdNr

    constructor {Type IdPrefix IdPrefixBindings {TypeSubst ""}} {
	set type $Type
	
	set idPrefix $IdPrefix
	set nextIdNr 0
	
	
	set objects [dict create]
	set userBindings [::loon::classes::CollectionBindings new\
			      $Type "${IdPrefixBindings}Binding"\
			      FALSE $TypeSubst]
	set systemBindings [::loon::classes::CollectionBindings new\
				$Type "${IdPrefixBindings}Systembinding"\
				TRUE $TypeSubst]
	
    }
    
    ## say waht the next id would be
    method nextId {} {
	return 	"${idPrefix}${nextIdNr}"
    }
    
    method add {object {label ""} {type ""}} {
	set id "${idPrefix}${nextIdNr}"
	incr nextIdNr
	dict set objects $id [dict create object $object label $label type $type]
	my NotifyBindings [self] $id add
	#puts "[my varname objects]"
	return $id
    }
    
    method delete {id} {
	my errorIfIdNotExists $id
	[dict get $objects $id object] destroy
	dict unset objects $id
	my NotifyBindings [self] $id delete
    }
    
    method use {id args} {
	my errorIfIdNotExists $id
	return [[dict get $objects $id object] {*}$args]
    }
    method with {id args} { my use $id {*}$args } 
    method target {id args} { my use $id {*}$args }
    
    
    method getObject {id} {
	my errorIfIdNotExists $id
	return [dict get $objects $id object]
    }
    
    method getType {id} {
	my errorIfIdNotExists $id
	return [dict get $objects $id type]
    }

    method getLabel {id} {
	my errorIfIdNotExists $id
	return [dict get $objects $id label]
    }
    
    
    method get {} {
	return [dict keys $objects]
    }
    method ids {} {
	return [dict keys $objects]
    }
    method list {} {
	return [dict keys $objects]
    }
    
    method relabel {id label} {
	my errorIfIdNotExists $id
	
	if {[dict get $objects $id label] ne $label} {
	    dict set objects $id label $label
	    my NotifyBindings [self] $id relabel
	}
    }
    
    method addSubstitutions {subst} {
	$userBindings addSubstitutions $subst
	$systemBindings addSubstitutions $subst
    }


    method setSubstitutions {subst} {
	$userBindings setSubstitutions $subst
	$systemBindings setSubstitutions $subst
    }
    
    method NotifyBindings {object id events} {
	$systemBindings notify $object $id $events
	$userBindings notify $object $id $events
    }
    
    
    method bind {systemOrUser cmd args} {
	switch -- $cmd {
	    ids {
		[set ${systemOrUser}Bindings] get
	    }
	    delete -
	    reorder -
	    get - 
	    add {
		[set ${systemOrUser}Bindings] $cmd {*}$args
	    }
	    notify {
		error "notify bindings are not possible."
	    }
	    default {
		puts "bind $cmd, use add, [info level 0] & [info level -1]"
		[set ${systemOrUser}Bindings] add $cmd {*}$args
	    }		    
	}
    }
    
    method errorIfIdNotExists {id} {
	if {![dict exists $objects $id]} {
	    error "$type \"$id\" does not exist."
	}
    }
    
}
