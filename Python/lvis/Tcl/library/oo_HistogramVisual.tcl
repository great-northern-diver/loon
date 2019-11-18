
::oo::class create ::loon::classes::HistogramVisual { 
    
    superclass ::loon::classes::LayerVisual

    variable ids modelns bins_var origin_var binwidth_var\
	showOutlines_var showStackedColors_var colorFill_var colorOutline_var\
	color_var showBinHandle_var\
	colorStackingOrder_var\
	idorigin idbinwidth idconnect\
	binalgRun
    

    constructor {Layerobj args} {

	set binalgRun ""
	set ids [dict create]
	
	set modelns [set [info object namespace $Layerobj]::modelns]
	
	foreach state {count bins binwidth origin color colorStackingOrder\
			   showOutlines showStackedColors showBinHandle colorFill colorOutline} {
	    set ${state}_var [uplevel #0 [list ${modelns}::my varname $state]]
	}
	
	next $Layerobj {*}$args
    }

    
    destructor {
	my variable statebinding
	if {$statebinding ne ""} {
	    catch {my clear}
	    catch {${modelns}::my systembind state delete $statebinding}
	    set statebinding ""
	}	
	next
    }

    
    method MakeStateBinding {} {
	my variable statebinding
	set statebinding [${modelns}::my systembind state add all "[self] layerchanged %e"]
    }
    

    
    method layerupdate {Events} {

	## Copy events in a dictionary
	## for fast access of events
	set events [dict create]
	foreach e $Events {
	    dict set events $e 1
	}

	if {[dict exists $events destroy]} {
	    puts "destroy histogram"
	    return
	}
	
	## Plot_View takes care of zoom and pan
	set nevents [llength $Events]
	set numHits 0
	foreach e {panX panY zoomX zoomY deltaX deltaY} {
	    if {[dict exists $events $e]} {incr numHits}
	}
	if {$numHits >= $nevents} {return}
	
	my LayerUpdateDict $events
    }
    
    method LayerUpdateDict {events} {
	my redraw
    }
    
    method clear {} {
	my variable visualid canvas	 
	uplevel #0 [list $canvas delete $visualid]
	set ids ""
    }
    
    method redraw {} {
	my variable canvas visualid

	my clear
	
	if {[set $showOutlines_var]} {
	    set outline black
	} else {
	    set outline ""
	}
	

	set binalgRun [dict get [set $bins_var] binalgRun]
	
	set selcolor $::loon::Options(select-color)

	set ids [dict create]
	
	foreach binid [dict with $bins_var {dict keys $bin}] {

	    if {[set $showStackedColors_var]} {		
		set counts [dict get [set $bins_var] bin $binid count] 		
		dict for {color num} $counts {
		    if {!($color eq "all" || $color eq "selected")} {
			dict set ids $binid $color\
			    [uplevel #0 [list $canvas create rect 0 0 0 0\
					     -fill $color -outline ""\
					     -tag [list layer $visualid "bin$binid" $color]]]
		    }
		}
	    } else {
		dict set ids $binid allFill\
		    [uplevel #0 [list $canvas create rect 0 0 0 0\
				     -fill thistle -outline ""\
				     -tag [list layer $visualid "bin$binid" all]]]
	    }
	    
	    if {[dict get [set $bins_var] bin $binid count selected] > 0} {
		dict set ids $binid selected\
		    [uplevel #0 [list $canvas create rect 0 0 0 0\
				     -fill $selcolor -outline ""\
				     -tag [list layer $visualid "bin$binid" selected]]]
	    }
	    
	    if {[set $showOutlines_var]} {
		dict set ids $binid allOutline\
		    [uplevel #0 [list $canvas create rect 0 0 0 0\
				     -fill "" -outline $outline\
				     -tag [list layer $visualid "bin$binid" all]]]
	    }
	}

	
	
	## origin and binwidth
	if {[set $showBinHandle_var]} {
	    my DrawBinHandle	    
	}
	
	my updateCoords
	next ;## move layer into correct place
    }

    method DrawBinHandle {} {
	my variable canvas visualid
	set idconnect [uplevel #0 [list $canvas create line 0 0 0 0 -fill black -width 1\
				       -tag [list layer $visualid "connect"]]]
	
	
	set idorigin [uplevel #0 [list $canvas create rect 0 0 0 0\
				      -fill grey70 -outline black -width 1\
				      -tag [list layer $visualid "handle_origin"]]]
	
	set idbinwidth [uplevel #0 [list $canvas create polygon 0 0 0 0\
					-fill grey70 -outline black -width 1\
					-tag [list layer $visualid "handle_binwidth"]]]
    }





    method updateCoords {} {
	my variable canvas map
	
	
	set bins [set $bins_var]

	## The *_View classes also call this method, for example for a
	## swapAxes event. If the swapAxes event was combined with any
	## other event that changes the binning, e.g. selected or
	## active, then this method will try to update the wrong	
	if {[dict get $bins binalgRun] ne $binalgRun} {
	    my redraw
	    return
	}
	
	set showStackedColors [set $showStackedColors_var]
	set showOutlines [set $showOutlines_var]
	
	
	if {$showStackedColors} {
	    set colorStacking [set $colorStackingOrder_var]
	    set allcols [lsort -unique [set $color_var]]
	    foreach col $allcols {
		if {$col ni $colorStacking} {
		    lappend colorStacking $col
		}
	    }
	}

	
	dict for {id vals} $ids {
	    
	    set xrange 	 [list [dict get $bins bin $id x0]\
			      [dict get $bins bin $id x1]]
	    
	    set coordsAll [$map mapDxy2Scoords $xrange\
			       [list 0 [dict get $bins bin $id count all]]]
	    


	    if {$showStackedColors} {
		set counts [dict get $bins bin $id count] 	
		
		set previousCount 0
		## selected is considered a color too
		foreach col $colorStacking {
		    if {[dict exists $counts $col]} {
			set count [dict get $counts $col]
			
			if {$col eq "selected" && ($count eq 0 || $count eq 0.0)} {
			    continue
			}
			set newcount [expr {$previousCount + $count}]
			uplevel #0 [list $canvas coords [dict get $vals $col]\
					[$map mapDxy2Scoords $xrange\
					     [list $previousCount $newcount]]]
			
			set previousCount $newcount
		    }
		    
		}
	    } else {
		uplevel #0 [list $canvas coords [dict get $vals allFill] $coordsAll]
		
		set count_selected  [dict get $bins bin $id count selected]
		
		if {$count_selected > 0} {
		    uplevel #0 [list $canvas coords [dict get $vals selected]\
				    [$map mapDxy2Scoords $xrange\
					 [list 0 $count_selected]]]
		}
		
	    }
	     		    
	    
	    
	
	    if {$showOutlines} {
		uplevel #0 [list $canvas coords [dict get $vals allOutline] $coordsAll]
	    }
	}


	## origin and binwidth
	if {[set $showBinHandle_var]} {
	    my UpdateBinHandle
	}
	
    }

    method UpdateBinHandle {} {
	my variable canvas map
	
	set binwidth [set $binwidth_var]
	set origin [set $origin_var]
	
	set xy_origin [$map mapDxy2Sxy $origin 0]
	set xy_bin [$map mapDxy2Sxy [expr {$origin+$binwidth}] 0]

	## these are in canvas coordinates
	set xo [lindex $xy_origin 0]
	set yo [lindex $xy_origin 1]
	set xb [lindex $xy_bin 0]
	set yb [lindex $xy_bin 1]
	
	
	if {[$map getSwap]} {
	    
	    set yo0 $yo ;#[expr {$yo - 4}]
	    set yo1 [expr {$yo + 8}]
	    set yb0 $yb 
	    set yb1 [expr {$yb + 8}]
	    
	    set x0 [expr {$xo-10}]
	    set x1 [expr {$xo-30}]
	    set x05 [expr {$xo-20}]
	    
	    uplevel #0 [list $canvas coords $idorigin $x0 $yo0 $x1 $yo1]
	    uplevel #0 [list $canvas coords $idbinwidth $x0 $yb1 $x05 $yb0 $x1 $yb1]
	    uplevel #0 [list $canvas coords $idconnect $x05 $yo1 $x05 $yb0]
	    
	} else {
	    
	    set xo0 $xo; #[expr {$xo - 4}]
	    set xo1 [expr {$xo + 8}]
	    set xb0 [expr {$xb - 8}]
	    set xb1 $xb
	    
	    set y0 [expr {$yo+10}]
	    set y1 [expr {$yo+30}]
	    set y05 [expr {$yo+20}]	    
	
	    uplevel #0 [list $canvas coords $idorigin $xo0 $y0 $xo1 $y1]
	    uplevel #0 [list $canvas coords $idbinwidth $xb0 $y0 $xb1 $y05 $xb0 $y1]
	    uplevel #0 [list $canvas coords $idconnect $xo1 $y05 $xb0 $y05]
	}
	
	


	
	
	
    }

    
    method updateZoomPan {oldPanX oldPanY oldZoomX oldZoomY} {
	my updateCoords
    }

 
    method indexWithCurrentTag {} {
	my variable canvas visualid

	set tags [$canvas gettags current]	
	
	set bins [set $bins_var]
	set tagbinid [lindex $tags 2]
	set showStackedColors [set $showStackedColors_var]

	if {[lindex $tags 1] eq $visualid && [string range $tagbinid 0 2] eq "bin"} {
	    set binid [string range $tagbinid 3 end]  
	    set tagcolor [lindex $tags 3]

	    if {!$showStackedColors && $tagcolor eq "all"} {
		return [::loon::listfns::setDiff\
			    [dict get $bins bin $binid points all]\
			    [dict get $bins bin $binid points selected]]
	    } elseif {$showStackedColors && $tagcolor eq "all"}  {
		return {}
	    } else {
		return [dict get $bins bin $binid points $tagcolor]
	    }
	} else {
	    return {}
	}

    }
    
    method indecesWithinRect {x0 y0 x1 y1} {
	my variable canvas map
	
	set vpx0 [$map getVpx0]; set vpx1 [$map getVpx1]
	set vpy0 [$map getVpy0]; set vpy1 [$map getVpy1]
	if {$x0 < $x1} {
	    if {$x0 > $vpx0} { set x0rect $x0 } else { set x0rect $vpx0 }
	    if {$x1 < $vpx1} { set x1rect $x1 } else { set x1rect $vpx1 }
	} else {
	    if {$x1 > $vpx0} { set x0rect $x1 } else { set x0rect $vpx0 }
	    if {$x0 < $vpx1} { set x1rect $x0 } else { set x1rect $vpx1 }
	}
	if {$y0 < $y1} {
	    if {$y0 > $vpy0} { set y0rect $y0 } else { set y0rect $vpy0 }
	    if {$y1 < $vpy1} { set y1rect $y1 } else { set y1rect $vpy1 }
	} else {
	    if {$y1 > $vpy0} { set y0rect $y1 } else { set y0rect $vpy0 }
	    if {$y0 < $vpy1} { set y1rect $y0 } else { set y1rect $vpy1 }
	}
	set inRect [$canvas find overlapping $x0rect $y0rect $x1rect $y1rect]

	
	return [my ItemIndicesForCanvasIds $inRect]
    }
    
    method ItemIndicesForCanvasIds {cids} {
	my variable canvas visualid view
	
	if {[llength $ids] eq 0} {return {}}
	
	set indices {}
	
	set bins [set $bins_var]
	set showStackedColors [set $showStackedColors_var]
	
	foreach id $cids {
	    set tags [$canvas gettags $id]
	    
	    set tagbinid [lindex $tags 2]
	    
	    if {[lindex $tags 1] eq $visualid && [string range $tagbinid 0 2] eq "bin"} {
		set binid [string range $tagbinid 3 end]
		set tagcolor [lindex $tags 3]
		if {$showStackedColors && $tagcolor eq "all"} { continue }
		lappend indices {*}[dict get $bins bin $binid points $tagcolor]
	    }
	}
	#	## certain glyphs migth have more than one
	#	return [lsort -unique $indices]
	return $indices
    }
   
}
