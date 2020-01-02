
oo::class create loon::classes::GlyphsInspector {

    superclass ::loon::classes::Inspector2

    variable treeview glyphTypes\
	activeGlyphInspector activeGlyph
    
    constructor {Path} {
	
	set activeGlyphInspector ""
	
	set glyphTypes {image serialaxes pointrange text spiro polygon}
	
	next $Path glyph "%g"
    }

    method ChangeGlyph {glyph} {
	my variable activewidget path
	
	if {$activewidget eq ""} { return }
	
	uplevel #0 [list ${path}.${activeGlyphInspector}.inspector\
			configure -activewidget "" -activeglyph ""]
	
	if {$glyph ne ""} {
	    if {$activeGlyphInspector ne ""} {
		pack forget ${path}.$activeGlyphInspector
	    }
	    
	    set activeGlyphInspector [$activewidget glyph type $glyph]	
	    uplevel #0 [list ${path}.${activeGlyphInspector}.inspector\
			    configure -activewidget $activewidget -activeglyph $glyph]
	    
	    pack ${path}.$activeGlyphInspector -fill x -padx 5 -pady 5
	}
    }


    method GlyphSelected {} {
	my ChangeGlyph [$treeview selection]
    }
    
    method ActivewidgetEvents {event glyph} {
	my variable activewidget
	
	switch -- $event {
	    add {
		$treeview insert "" end -id $glyph\
		    -text [$activewidget glyph getLabel $glyph]\
		    -values [list [$activewidget glyph getType $glyph] $glyph]
	    }
	    delete {
		$treeview delete $glyph
	    } 
	    relabel {
		$treeview item $glyph -text [$activewidget glyph getLabel $glyph]
	    }
	}
	
#	puts $treeview
	set id [$treeview selection]
	if {$id eq "" && [llength [$treeview children ""]]} {
	    set glyph [lindex [$treeview children ""] 0]
	    $treeview selection set $glyph
	    my ChangeGlyph $glyph
	}
	
    }

    method RegisterActivewidget {} {
	my variable activewidget
	next
      
	
	## repopulate
	if {$activewidget ne ""} {
	    set glyphs [$activewidget glyph ids]

	    foreach glyph $glyphs {
		$treeview insert "" end -id $glyph\
		    -text [$activewidget glyph getLabel $glyph]\
		    -values [list [$activewidget glyph getType $glyph] $glyph]
	    }
	    
	    if {[llength $glyphs] > 0} {
		$treeview selection set [lindex $glyphs 0]
		my ChangeGlyph [lindex $glyphs 0]
	    }
	} 
    }
    
    method UnregisterActivewidget {} {
	my variable activewidget path
	
	catch {$treeview delete [$treeview children ""]}
	
	next
	if {$activewidget ne ""} {
	    catch {uplevel #0 [list ${path}.${activeGlyphInspector}.inspector\
				   configure -activewidget "" -activeglyph ""]}
	}
    }
    
    
    method CheckNewActivewidget {widget} {
	if {![info object isa typeof $widget\
		  "::loon::classes::withGlyphs"]} {
	    error "$widget does not support glyphs."
	}
    }

    method Make {} {
	my variable path
	
	frame $path -class GlyphInspector
	
	pack [frame ${path}.glyphselector] -fill x -side top
	
	set treeview [::ttk::treeview ${path}.glyphselector.treeview\
			  -height 10 -selectmode browse\
			  -columns {type id}]
	
	$treeview heading #0 -text label -anchor c
	$treeview heading type -text type
	$treeview heading id -text id
	$treeview column type -width 80 -stretch FALSE
	$treeview column id -width 60 -stretch FALSE
	$treeview column #0 -width 100 -stretch TRUE


	set scroll ${path}.glyphselector.scroll
	pack $treeview -side left -anchor n -fill x -expand 1 -padx {5 0} -pady 5
	pack [::tk::scrollbar $scroll\
		 -orient vertical -command "$treeview yview"]\
	    -side left -anchor n -fill y -padx {0 5} -pady 5
	
	
	$treeview configure -yscrollcommand "$scroll set"
		
	bind $treeview <<TreeviewSelect>> "[self namespace]::my GlyphSelected"
	
	
	## create particular glyph inspectors
	foreach type $glyphTypes {
	    ::tk::labelframe ${path}.$type -text [string toupper $type 0] -padx 2 -pady 2
	    
	    pack [::loon::${type}_glyph_inspector ${path}.${type}.inspector]\
		-fill x
	}
	pack ${path}.${type} -fill x -padx 5 -pady 5
	set activeGlyphInspector $type
	
    } 
    
    
    
    
}
