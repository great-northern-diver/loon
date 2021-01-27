
::oo::class create ::loon::classes::Histogram_View {

    superclass ::loon::classes::withCanvasAndItemBindings\
	::loon::classes::Decorated_View


    variable controller

    constructor {Path} {

	next $Path

	set controller [::loon::classes::Histogram_Controller new [self]]
    }

    ## Also add layers for
    ## - scales, guides
    ## - decorations (title, xlabel, ylabel, border)
    ## - keep those updated


    method setPlotModel {Model} {
	next $Model
	$controller setModel $Model
    }


}
