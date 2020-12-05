::oo::class create ::loon::classes::Plot_Model {

    superclass\
	::loon::classes::withStateBindings\
	::loon::classes::Linkable


    constructor {} {

	next


	my New_state zoomX positive_double 1 1
	my New_state zoomY positive_double 1 1
	my New_state panX double 1 0
	my New_state panY double 1 0
	my New_state deltaX positive_double 1 1
	my New_state deltaY positive_double 1 1

	my New_state xlabel string 1 ""
	my New_state ylabel string 1 ""
	my New_state title string 1 ""

	my New_state showLabels boolean 1 TRUE
	my New_state showScales boolean 1 FALSE
	my New_state swapAxes boolean 1 FALSE
	my New_state showGuides boolean 1 FALSE

	my New_state background color 1 $::loon::Options(background)
	my New_state foreground color 1 $::loon::Options(foreground)

	my New_state guidesBackground color 1 $::loon::Options(guidesBackground)
	my New_state guidelines color 1 $::loon::Options(guidelines)

	## in R usually bottom left top right: s w n e
	my New_state minimumMargins positive_integer 4 [lrepeat 4 20]
	my New_state labelMargins positive_integer 4 {30 30 60 0}
	my New_state scalesMargins positive_integer 4 {30 80 0 0}


	## State Descriptions
	my SetStateDescription zoomX\
	    "Data to plot region mapping is x_pr = (x_data-panX)/deltaX*zoomX if swapAxes=FALSE. The plot region is a plane defined by (0,0) and (1,1)."
	my SetStateDescription zoomY\
	    "Data to plot region mapping is y_pr = (y_data-panY)/deltaY*zoomY if swapAxes=FALSE. The plot region is a plane defined by (0,0) and (1,1)."
	my SetStateDescription deltaX\
	    "Data to plot region mapping is x_pr = (x_data-panX)/deltaX*zoomX if swapAxes=FALSE. The plot region is a plane defined by (0,0) and (1,1)."
	my SetStateDescription deltaY\
	    "Data to plot region mapping is y_pr = (y_data-panY)/deltaY*zoomY if swapAxes=FALSE. The plot region is a plane defined by (0,0) and (1,1)."
	my SetStateDescription panX\
	    "panX is the visible x-axis origin"
	my SetStateDescription panY\
	    "panY is the visible y-axis origin"

	my SetStateDescription xLabel\
	    "x axis label"
	my SetStateDescription yLabel\
	    "y axis label"
	my SetStateDescription title\
	    "plot title"

	my SetStateDescription showLabels\
	    "boolean to specify whether to display the axes labels and title or not"
	my SetStateDescription showScales\
	    "boolean to specify whether to display the scales or not"
	my SetStateDescription swapAxes\
	    "boolean for swapping the x and y axes"

	my SetStateDescription showGuides\
	    "guides a horizontal and vertical lines for visual guidance "

	my SetStateDescription background\
	    "background color of plot"
	my SetStateDescription foreground\
	    "foreground color of plot including label, axes and scales color"

	my SetStateDescription guidesBackground\
	    "background color if showGuides=TRUE"

	my SetStateDescription guidelines\
	    "guidelines color"

	my SetStateDescription minimumMargins\
	    "bottom, left, top, right minimum margin in pixels"
	my SetStateDescription labelMargins\
	    "bottom, left, top, right margins for labels in pixels"
	my SetStateDescription scalesMargins\
	    "bottom, left, top, right margins for scales in pixels"

    }

    destructor {
	next
    }


    ## Check that deltaX and deltaY are not too small
    method EvalConfigure {} {
	my variable confDict

	next

	if {[dict exists $confDict new_deltaX]} {
	    if {[dict get $confDict new_deltaX] < 0.0005} {
		::loon::warning "deltaX is too small, changed to 0.0005"
		dict set confDict new_deltaX 0.0005
	    }
	}

	if {[dict exists $confDict new_deltaY]} {
	    if {[dict get $confDict new_deltaY] < 0.0005} {
		::loon::warning "deltaY is too small, changed to 0.0005"
		dict set confDict new_deltaY 0.0005
	    }
	}

    }

    method isWidget {} {
	return FALSE
    }

}
