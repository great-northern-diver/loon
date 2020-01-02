
namespace eval loon {
    
    proc metaKeyGuiChangeKey {tt action meta} {
	
	set gc $tt.f.gc
	set pc $tt.f.pc
	
	$gc.k$action configure -text $meta

	set pan [$gc.kpan cget -text]
	set mulSel [$gc.kmulSel cget -text]
	set move [$gc.kmove cget -text]	
	
	if {$action eq "pan"} {
	    if {$meta eq $mulSel} {
		$gc.kmulSel configure -text "-"
	    }
	    if {$meta eq $move} {
		$gc.kmove configure -text "-"
	    }
	}

	if {$action eq "mulSel"} {
	    if {$meta eq $pan} {
		$gc.kpan configure -text "-"
	    }
	    if {$meta eq $move} {
		$gc.kmove configure -text "-"
	    }
	}

	if {$action eq "move"} {
	    if {$meta eq $pan} {
		$gc.kpan configure -text "-"
	    }
	    if {$meta eq $mulSel} {
		$gc.kmulSel configure -text "-"
	    }
	}

	::loon::metaUpdateActiveButton $gc $pc
	
    }
    
    proc metaUpdateActiveButton {gc pc} {
	
	set pan [$gc.kpan cget -text]
	set mulSel [$gc.kmulSel cget -text]
	set move [$gc.kmove cget -text]	

	if {$pan eq $::loon::metaPan &&\
		$mulSel eq $::loon::metaMulSel &&\
		$move eq $::loon::metaTempMove} {
	    $pc.bapply configure -state disabled
	} else {
	    $pc.bapply configure -state normal
	}
    }
}

proc loon_setMetaKeys {args} {

    # Save arguments in a dict
    if {[catch {set argDict [dict create {*}$args]}]} {
	error "need key value pairs"
    }
    
    foreach action {pan move multipleSelect} {

	if {[dict exists $argDict "-$action"]} {
	    
	    set meta [dict get $argDict "-$action"]
	    
	    if {$meta in {"" "-"}} {
		set meta$action omit
		continue
	    } 
	    
	    set meta [string toupper\
			  [string tolower $meta]\
			  0 0]
	    
	    if {$meta ni {Command Shift Ctrl Control Alt Option}} {
		error "meta key must be one of: Command Shift Ctrl Control Alt Option"
	    }
	    
	    if {$meta eq "Ctrl"} {
		set meta Control
	    }
	    
	    set meta$action $meta

	} else {
	    set meta$action -1
	}
    }

    
    if {$metamultipleSelect ne "-1"} {
	set ::loon::metaMulSel $metamultipleSelect
    }
    
    if {$metamove ne "-1"} {
	if {$::loon::metaMulSel eq $metamove} {
	    ::loon::warning "meta key for temporary move is the same as for multiple select and hence will be omitted."
	    set ::loon::metaTempMove omit
	} else {
	    set ::loon::metaTempMove $metamove
	}
    }
    
    if {$metapan ne "-1"} {
	if {$::loon::metaMulSel eq $metapan} {
	    ::loon::warning "meta key for pan is the same as for multiple select and hence will be omitted."
	    set ::loon::metaPan omit
	} elseif {$::loon::metaTempMove eq $metapan} {
	    ::loon::warning "meta key for pan is the same as for temporary moving points and hence will be omitted."
	    set ::loon::metaPan omit
	} else {
	    set ::loon::metaPan $metapan
	}
    }
    
    ## Bind all open displays with new bindings
    set plotWidgets [array names ::loon::OpenWidgets]
    
    #foreach plotWidget $plotWidgets {
    #if {$::loon::OpenWidgets($plotWidget) eq "plot"} {
    #	    ::loon::plot::bindCanvas $plotWidget
    #}
    #}

    
    ## Re-Bind Selct by Color Chooser
    set colorChooser $::loon::inspector.plot.select.selectcolor.colors.canvas
    
    foreach meta {Command Shift Control Alt Option} {
	$colorChooser bind "color" <$meta-Button-1> {}
    }

    if {$::loon::metaMulSel ne "omit"} {
	$colorChooser bind "color" <$::loon::metaMulSel-Button-1>\
	    $::loon::plot::inspector::metaSelectByColorCommand
    }
}

proc loon_setMetaKeysGUI {} {
    
    ## creat new toplevel window
    set id 0
    while {[winfo exists .loon_$id] eq 1} {
	incr id
    }
    set tt [toplevel .loon_$id]
    wm title $tt "Set Meta Keys"
    
    set tf [frame $tt.f]
    pack $tf -side top -fill both -expand 1 -padx 10 -pady 10
    
    ## grid containder
    set gc [frame $tf.gc]
    ## pack container
    set pc [frame $tf.pc]

    pack $gc -side top -fill x -expand 1 -anchor nw -pady {0 5}
    
    pack $pc -side top -anchor center

    set row 1
    foreach name {pan mulSel move}\
	label {"Pan" "Multiple Selection" "Move Points"}\
	key \
	[list $::loon::metaPan $::loon::metaMulSel\
	     $::loon::metaTempMove] {
		 
		 set lab [label $gc\.l$name -text $label -justify left]
		 grid $lab -row $row -column 0 -sticky "w" -padx 5
		 
		 set kl [label $gc\.k$name -text $key]
		 grid $kl -row $row -column 1 -sticky "w" -padx 5
		 
		 ## bind
		 foreach meta $::loon::metaKeys {
			 bind $lab <$meta-Button-1>\
			     "::loon::metaKeyGuiChangeKey\
                                  $tt $name $meta"
			 bind $kl <$meta-Button-1>\
			     "::loon::metaKeyGuiChangeKey\
                                  $tt $name $meta"
		     }
		 
		 incr row
	     }
    
    set lexpl [label $gc.expl\
		   -text "Hold down a meta key and click on a label\nto\
                              assign a new meta key to that action."\
		   -justify left]
    
    grid $lexpl -row 0 -column 0 -columnspan 3 -sticky w -pady {0 5}

    grid columnconfigure $gc 0 -weight 0
    grid columnconfigure $gc 1 -weight 1
    grid columnconfigure $gc 2 -weight 0
    
    ## Buttons
    button $pc.bclose -text "Close"
    button $pc.bapply -text "Apply" -state disabled
    button $pc.bdefault -text "Use Default"
    
    pack $pc.bdefault $pc.bapply $pc.bclose \
	-side left -anchor n

    # Button Actions
    $pc.bdefault configure -command\
	"$gc\.kpan configure -text \$::loon::defaultMetaPan
	    $gc\.kmulSel configure -text \$::loon::defaultMetaMulSel
	    $gc\.kmove configure -text \$::loon::defaultMetaTempMove
            ::loon::metaUpdateActiveButton $gc $pc"

    
    $pc.bclose configure -command "destroy $tt"

    $pc.bapply configure -command "
            $pc.bapply configure -state disabled\n\
            loon_setMetaKeys\
		-pan \[$gc\.kpan cget -text\]\
		-move \[$gc\.kmove cget -text\]\
		-multipleSelect \[$gc\.kmulSel cget -text\]"
    
}
