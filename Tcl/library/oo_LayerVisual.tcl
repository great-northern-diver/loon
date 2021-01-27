
::oo::class create ::loon::classes::LayerVisual {

    superclass ::loon::classes::Visual

    variable layerobj statebinding container tag_var x_var y_var

    ## layerid is visualid
    constructor {Layerobj Container args} {

    set classes [info object class $Layerobj]

	set layerobj $Layerobj
	set statebinding ""
	set container $Container

	if {[info object isa typeof $Layerobj\
		 ::loon::classes::ModelLayer]} {
	    set ns [set [info object namespace $Layerobj]::modelns]
	} else {
	    set ns [info object namespace $Layerobj]
	}

	foreach state {x y tag} {
	    set ${state}_var [uplevel #0 [list ${ns}::my varname $state]]
	}

	my MakeStateBinding

	next {*}$args

    }

    method MakeStateBinding {} {
      	set statebinding [$layerobj systembind state add all "[self] layerchanged %e"]
    }

    destructor {
	if {$statebinding ne ""} {
	    ## destroy gets called if the window gets cloesed
	    ## or the layer object gets deleted
	    catch {$layerobj systembind state delete $statebinding}
	}
	next
    }

    method layerchanged {events} {
	if {$events eq "destroy"} {
	    my destroy
	} else {
	    my layerupdate $events
	}
    }
    method layerupdate {events} {
	my redraw
    }


    method InfoDebug {args} {
	next layerobj statebinding {*}$args
    }

    method redraw {} {
	my variable visualid
	## visualid is also the layer id
	$container layerUpdate $visualid move
    }

    method updateZoomPan {oldPanX oldPanY oldZoomX oldZoomY} {
	my updateCoords
    }

}
