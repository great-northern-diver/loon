
## \brief A function to find all the top most widgets (only)
##
namespace eval loon {
    proc listTopWidgets {{W .}} {

    set list {}

    if { [string equal [winfo toplevel $W] $W] } {
        lappend list $W
    }

    foreach w [winfo children $W] {
        set list [concat $list  $w]
    }

    return $list

    }
}
