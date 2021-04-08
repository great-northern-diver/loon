

## Plot
option add *LoonPlot.canvas.background white widgetDefault
option add *LoonPlot.canvas.width 400 widgetDefault
option add *LoonPlot.canvas.height 250 widgetDefault




## LayersInspector
option add *LoonLayersInspector.visible_color black
option add *LoonLayersInspector.notvisible_color gray70
option add *LoonLayersInspector.partlyvisible_color "forest green"


## PlotInspector
option add *LoonPlotInspector*modify*padY 2
option add *LoonPlotInspector*LoonPlotWorldview.canvas.height 160
option add *LoonPlotInspector*LoonPlotWorldview.canvas.width 80

namespace eval loon {
    variable Options
    variable pi
    variable cos30
    variable sin30
    variable sqrt3
    set pi 3.1415926535897931
    set cos30 [expr {cos($pi/6)}]
    set sin30 [expr {sin($pi/6)}]
    set sqrt3 [expr {sqrt(3.0)}]

    ## has some of the sizes
    variable circle_map

    proc warning {msg} {
	puts $msg
    }

    set Options(printConfigureDebug) FALSE
    set Options(printInConfigurationWarning) TRUE
    set Options(printInConfigurationWarningMsg) "setting loon::Options(printInConfigurationWarningMsg) to FALSE"

    ## abbreviations for states (alias)
    set Options(abbreviations) [dict create\
				    xlab xlabel\
				    ylab ylabel\
				    main title\
				    cex size\
				    col color\
				    lwd linewidth\
				    itemlabel itemLabel]


    if {[winfo screenheight .] > 750} {
	set Options(noscrollInspector) TRUE
    } else {
	set Options(noscrollInspector) FALSE
    }

    set ::loon::Options(image_scale) "::loon::image_scale"
    ## pre-calculated sizes
    set ::loon::Options(image_sizes) {1 2 3 4 5 6 7 8 9 10 15 20 40}

    ## Default foreground and background color
    set Options(color) gray60
    set Options(size) 4
    set Options(glyph) "ccircle"
    set Options(linewidth) 1

    ## Default foreground and background color
    set Options(background) white
    set Options(foreground) black
    set Options(guidesBackground) gray92
    set Options(guidelines) white
    set Options(facetLabelBackground) gray80

    #set Options(font) "Lucidia"
    set Options(font) "Helvetica"

    # "*-Helvetica-Medium-R-Normal-*-12-*"
    # "*-Helvetica-Bold-R-Normal-*-18-*"
    set Options(default-font) TkDefaultFont
    set Options(font-xlabel) $Options(default-font)
    set Options(font-ylabel) $Options(default-font)
    set Options(font-title) [font create {*}[font configure TkDefaultFont]]
    font configure $Options(font-title) -weight bold -size 16

    set Options(font-scales) $Options(default-font)
    set Options(font-text_glyph) $Options(default-font)

    set Options(export_ps_font) Helvetica



    set Options(select-color) magenta

    # suggested number of ticks per 100 pixels
    set Options(ticks_density) 1
    set Options(tick_length) 8

    set Options(canvas_bg_guides) gray93

    set Options(brush_color) lightgray
    set Options(brush_color_handle) gray

    ## Version 1
#    set Options(colors) [list \#377EB8 \#E41A1C \#4DAF4A \#984EA3\
#			     \#FF7F00 \#FFFF33 \#A65628 \#F781BF \#999999\
#			     \#8DD3C7 \#FFFFB3 \#BEBADA \#FB8072 \#80B1D3\
#			     \#FDB462 \#B3DE69 \#FCCDE5 \#BC80BD\
#			     \#CCEBC5 \#FFED6F]

    set Options(colors) [list "gray60" "#A6CEE3" "pink" "#B2DF8A" "#FDBF6F" "#FFFF99" "#CAB2D6" "#DEB887" "#CDC673" "lightgrey" "white" "gray10" "#1F78B4" "#E31A1C" "#33A02C" "#FF7F00" "#FFFF00" "#6A3D9A" "#B15928" "#8B864E" "gray40"]

    set Options(colors-palette) loon

    ##  ::loon::hcl::hclColorPaletteDialog 12 72 66 210

    ## set Options(colors) [::loon::hcl::hue_pal 12 {15 375} 72 66 210]
    set Options(colors-palette-hcl) {66 70 241}

    set Options(size_0_rect) 3

    set Options(fixed_guide_distance) 90


    ## Mouse Meta mapping
    if {$tcl_platform(os) eq "Linux"} {
	# on my machine Option is always pressed if NumLock is on
	set Options(metaKeys) {Command Shift Control Alt}
    } else {
	set Options(metaKeys) {Command Shift Control Alt Option}
    }

    set Options(metaMulSel) Shift
    if {$tcl_platform(os) eq "Darwin"} {
	set Options(metaTempMove) Control
    } else {
	set Options(metaTempMove) Control
    }

    set Options(defaultMetaTempMove) $Options(metaTempMove)
    set Options(defaultMetaMulSel) $Options(metaMulSel)

    proc map_size_A {size} {
	if {$size < 1} {
	    return 8.0
	} else {
	    return [expr {12.0*$size}]
	}
    }


    ## Glyph size mapping

    ## A = r^2*pi
    ## r = sqrt(A/pi)
    ##
    ## return r
    proc map_circle_size {size} {
	## do not use on list anymore
	set out {}
	foreach s $size {
	    set A [map_size_A $s]
	    lappend out [expr {sqrt($A/$::loon::pi)}]
	}
	return $out
    }


    ## A = 4s^2
    ## s = sqrt(A/4)
    ##
    ## return s
    proc map_square_size {size} {
	set A [map_size_A $size]
	return [expr {round(sqrt($A/4.0))}]
    }

    ## Equilateral triangle side a
    ## A = 1/4 a^2 sqrt(3)
    ## a = sqrt(4*A/sqrt(3))
    ##
    ## return a/2 1/3h 2/3h
    proc map_triangle_size {size} {

	set A [map_size_A $size]
	set a2 [expr {sqrt($A/$::loon::sqrt3)}]
	set h [expr {$a2*$::loon::sqrt3}]

	return [list $a2 [expr {$h/3.0}] [expr {$h*2.0/3}]]
    }


    ## A = 2*s^2
    ## s = sqrt(A/2)
    ##
    ## return s
    proc map_diamond_size {size} {
	set A [map_size_A $size]
	return [expr {round(sqrt($A/2.0))}]
    }

    ## Return Area
    proc map_image_size {size} {
	# to image area
	set out {}
	foreach s $size {
	    if {$size < 1} {
		lappend out 20
	    } else {
		lappend out [expr {600.0*$s}]
	    }
	}
	return $out
    }


    ## return radius
    proc map_star_size {size} {
	if {$size < 1} {
	    return 5
	} else {
	    return [expr {20*sqrt(double($size))}]
	}
    }

    ## Axes Distance
    proc map_pc_size {size} {
	if {$size <1} {
	    return 2
	} else {
	    return [expr {4*sqrt(double($size))}]
	}
    }

    proc map_text_size {size} {
	set out {}
	foreach s $size {
	    if {$s < 1} {
		lappend out 2
	    } else {
		lappend out [expr {2 + round($s)}]
	    }
	}
	return $out
    }


    ## currently as circle
    proc map_polygon_size {size} {
	## do not use on list anymore
	if {$size < 1} {
	    return 4
	} else {
	    return [expr {sqrt($size)*6}]
	}
    }



    ## scaling factor for spiro glyphs
    proc map_spiro_size {size} {
	if {$size <=1} {
	    return 3
	} else {
	    return [expr {4*$size}]
	}
    }

    proc getColorList {} {
	variable Options
	return $Options(colors)
    }

    proc setColorList {type args} {
	variable Options

	switch -- [string tolower $type] {
	    loon {
		set Options(colors) [list  "gray60" "#A6CEE3" "pink" "#B2DF8A" "#FDBF6F" "#FFFF99" "#CAB2D6" "#DEB887" "#CDC673" "lightgrey" "white" "gray10" "#1F78B4" "#E31A1C" "#33A02C" "#FF7F00" "#FFFF00" "#6A3D9A" "#B15928" "#8B864E" "gray40"]

		set Options(colors-palette) loon
	    }
	    hcl {
		set chroma [lindex $args 0]
		set luminance [lindex $args 1]
		if {[llength $args]>2} {
		    set hue_start [lindex $args 2]
		} else {
		    set hue_start 0
		}

		if {![::loon::listfns::isNumeric $chroma]} {
		    error "chroma: \"$chroma\" is not a numeric value."
		}

		if {$luminance < 0 || $luminance>100} {
		    error "luminance \"$luminance\" is not a valid value. Use a value in \[0, 100\]."
		}

		if {![::loon::listfns::isNumeric $hue_start]} {
		    error "hue_start: \"$hue_start\" is not a numeric value."
		}

		## chorma, luminance, hue_start
		set ::loon::hcl::memoisedColors ""
		set Options(colors-palette-hcl) [list $chroma $luminance $hue_start]
		set Options(colors) [::loon::hcl::hue_mem_pal 10 $chroma $luminance $hue_start]

		set Options(colors-palette) hcl
	    }
	    colorbrewer {
		set palette [lindex $args 0]
		switch -- [string tolower $palette] {
		    set1 {
			set Options(colors) [list "#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00" "#FFFF33" "#A65628" "#F781BF" "#999999"]
		    }
		    set2 {
			set Options(colors) [list "#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"]
		    }
		    set3 {
			set Options(colors) [list "#8DD3C7" "#FFFFB3" "#BEBADA" "#FB8072" "#80B1D3" "#FDB462" "#B3DE69" "#FCCDE5" "#D9D9D9" "#BC80BD" "#CCEBC5" "#FFED6F"]
		    }
		    pastel1 {
			set Options(colors) [list "#FBB4AE" "#B3CDE3" "#CCEBC5" "#DECBE4" "#FED9A6" "#FFFFCC" "#E5D8BD" "#FDDAEC" "#F2F2F2"]
		    }
		    pastel2 {
			set Options(colors) [list  "#B3E2CD" "#FDCDAC" "#CBD5E8" "#F4CAE4" "#E6F5C9" "#FFF2AE" "#F1E2CC" "#CCCCCC"]
		    }
		    paired {
			 # set Options(colors) [list "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"]
			 set Options(colors) [list "#1F78B4" "#33A02C" "#E31A1C" "#FF7F00" "#6A3D9A" "#B15928" "#A6CEE3" "#B2DF8A" "#FB9A99" "#FDBF6F" "#CAB2D6" "#FFFF99"]
		    }
		    dark2 {
			set Options(colors) [list "#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D" "#666666"]
		    }
		    accent {
			set Options(colors) [list "#7FC97F" "#BEAED4" "#FDC086" "#FFFF99" "#386CB0" "#F0027F" "#BF5B17" "#666666"]
		    }
		    default {
			error "$type pallete $palette not known. Use on of:\
                               Set1, Set2, Set3,\
                               Pastel1, Pastel2, Paired, Dark2, Accent."
		    }
		}
		set Options(colors-palette) ColorBrewer

	    }
	    ggplot2 {
		if { [llength $args] > 0 } {
		    ## use n as first argument
		    set n [lindex $args 0]
		} else {
		    set n 10
		}

		set Options(colors) [::loon::hcl::hue_pal $n]
		set Options(colors-palette) ggplot2
	    }
	    baser {
		set Options(colors) {"#000000" "#FF0000" "#00CD00" "#0000FF" "#00FEFE" "#FF00FF" "#FFFF00" "#BEBEBE"}
		set Options(colors-palette) baseR

	    }
	    custom {
		if {[::loon::listfns::isColor {*}$args]} {
		    set Options(colors) {*}$args
		} else {
		    error "not all colors are valid specifiers in: $args."
		}
		set Options(colors-palette) custom
	    }
	    default {
		error "type \"$type\" not valid. Choose one of: ColorBrewer, hcl, custom."
	    }
	}

	if {$Options(colors-palette) eq "baseR" && $Options(select-color) eq "magenta"} {
	    set Options(select-color) orange
	} elseif {$Options(select-color) eq "orange"} {
	    set Options(select-color) magenta
	}

	return
    }


}
