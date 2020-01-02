
oo::class create loon::classes::PlotInspector {
    
    superclass ::loon::classes::Inspector2

    variable AnalysisInspector LayersInspector GlyphsInspector Worldview\
	vport vportid


    method CheckNewActivewidget {widget} {
	if {![info object isa typeof $widget\
		  "::loon::classes::Scatterplot_Widget"]} {
	    error "$widget is not a scatterplot widget."
	}
    }
    
    
    method RegisterActivewidget {} {
	my variable activewidget
	
	# no next, as we do not want statebindings
	#next
	$AnalysisInspector configure -activewidget $activewidget
	$LayersInspector configure -activewidget $activewidget
	$Worldview configure -activewidget $activewidget
	$GlyphsInspector configure -activewidget $activewidget
	
	
    }
    
    method analysisInspectorFactory {} {
        return ::loon::plot_inspector_analysis
    }
    
    method Make {}  {
	my variable path
	
	frame $path -class LoonPlotInspector
	
	## Create a Scatterplot Worldview
	
	set lworldview [labelframe ${path}.lworldview -text "World View" -padx 2 -pady 2]
	pack $lworldview -side top -fill x -pady 5 -padx 5
	set Worldview [::loon::worldview ${lworldview}.worldview ]
	pack $Worldview -fill both -expand TRUE
	
	set notebook [::ttk::notebook ${path}.notebook]

    # Use dynamic binding to determine the right analysis inspector
    # This way, the 3D plot can have its custom inspector
	if {$::loon::Options(noscrollInspector)} {
	    set AnalysisInspector\
		[[my analysisInspectorFactory] ${path}.analysis]  
	} else {
	    ## Scrollable analysis inspector
	    ## scroll form
	    set sf [frame ${path}.analysis]
	    set sfsb [::ttk::scrollbar ${sf}.sbar -command "${sf}.vport yview"]
	    pack $sfsb -side right -fill y
	    
	    set vport [canvas ${sf}.vport -yscrollcommand "${sf}.sbar set"]
	    pack $vport -side left -fill both -expand TRUE
	    
	    set AnalysisInspector\
		[[my analysisInspectorFactory] ${vport}.inspector]
	    
	    update idletasks
	    $vport configure -width [winfo reqwidth $AnalysisInspector]\
		-height 400
	    
	    set vportid [$vport create window 0 0 -anchor nw -window $AnalysisInspector]
	    
	    bind $AnalysisInspector <Configure> [list +[self namespace]::my analysisInspectorResize]
	    
	    bind $vport <Configure> +[list [self namespace]::my canvasResize]	    
	}


	set LayersInspector\
	    [::loon::layers_inspector ${notebook}.layers]

	set GlyphsInspector\
	    [::loon::glyphs_inspector ${notebook}.glyphs]
	
	$notebook add ${path}.analysis -text "Analysis"
	$notebook add $LayersInspector -text "Layers"
	$notebook add $GlyphsInspector -text "Glyphs"

	
	pack $notebook -fill both -expand TRUE -padx 0 -pady {2 0}
    }

    method canvasResize {} {
	set width [winfo width $vport]
	$vport itemconfigure $vportid -width $width
    }

    method analysisInspectorResize {} {
	set bbox [$vport bbox all]
	
	update idletasks
	$vport configure\
	    -scrollregion $bbox -yscrollincrement 0.1i
    }
    

}
