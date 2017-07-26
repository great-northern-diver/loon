
## Create a loon toplevel

namespace eval loon {

    proc loon_toplevel {} {
	set count 0
	set win ".l[incr count]"
	while {[winfo exists $win]} {
	    set win ".loon_[incr count]"
	}
	#log 1 "create a Loon toplevel: $win"
	::tk::toplevel $win -class Loon
    }

}
