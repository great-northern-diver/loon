
namespace eval loon {
    
    
    namespace export geodesic2d
    
    proc geodesic2d {args} {
	set obj [::loon::classes::Geodesic2d new]
	$obj configure {*}$args
	return $obj
    }
    
}
