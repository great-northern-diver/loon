oo::class create loon::classes::Serialaxes_Widget {

    superclass ::loon::classes::Plot_Widget\
	::loon::classes::Serialaxes_Model

    method InitView {} {
	my variable widgetpath widgetview

	set widgetview 	[::loon::classes::Serialaxes_View new $widgetpath]

	next
    }

    method BindCanvas {args} {
	my variable widgetview
	[info object namespace $widgetview]::my BindCanvas {*}$args
    }

    method BindItem {args} {
	my variable widgetview
	[info object namespace $widgetview]::my BindItem {*}$args
    }


    method AddSubstitutions {subst} {
	my variable widgetview
	[info object namespace $widgetview]::my AddSubstitutions $subst
	next $subst
    }

    method SetSubstitutions {subst} {
	my variable widgetview
	[info object namespace $widgetview]::my SetSubstitutions $subst
	next $subst
    }

    method redraw {args} {
 	my variable widgetview
	$widgetview redraw {*}$args
    }

}
