
::oo::class create ::loon::classes::Bindable {
    
    superclass ::loon::classes::fancyInfo
    
    method bind {type args} {
	my Bind user $type {*}$args
    }
    
    method systembind {type args} {
	my Bind system $type {*}$args
    }

    method Bind {systemOrUser type args} {
	set CMD "Bind[string toupper $type 0]"
	if {[catch {set res [my $CMD $systemOrUser {*}$args]} msg]} {
	    error $msg
	    my BindErrorMsg $type
	} else {
	    return $res
	}
    }

    
    method BindErrorMsg {type} {
	error "binding type \"$type\" is not supported.\
                       Valid types are: ..."
    }
    

        
}
