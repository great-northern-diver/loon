
oo::class create loon::classes::GraphInspectorAnalysis {

    superclass loon::classes::PlotInspectorAnalysis

    variable orbitVar

    method Make {} {
	my variable path

	next

	## Select
 	set sel ${path}.select.controls
	
	## Modify
	set modfg ${path}.modify.frame.glyphb
	
	set orbitVar ${modfg}.orbit.showOrbitVar
	set $orbitVar FALSE
	pack [checkbutton ${modfg}.orbit\
		  -image [image create bitmap\
			      -file [file join $::env(LOON_LIBRARY)\
					 images orbit.xbm]]\
		  -variable $orbitVar\
		  -onvalue TRUE -offvalue FALSE\
		  -command "[self namespace]::my ChangeOrbit"]\
	    -side left
	
    }
    
    method ChangeOrbit {} {
	my variable plotWidget
	if {$plotWidget ne ""} {
	    uplevel #0 [list $plotWidget configure -showOrbit [set ::$orbitVar]]
	}
    }
    
    method ActivewidgetEvents {args} {
	if {"showOrbit" in $args} {
	    my variable path plotWidget
	    
	    uplevel #0 [list set $orbitVar [$plotWidget cget -showOrbit]]
	}
	next {*}$args	
    }
    
    method RegisterActivewidget {} {
	my variable activewidget
	
	next
	
	if {$activewidget ne ""} {
	    my ActivewidgetEvents showOrbit
	}    
	
    }

}
