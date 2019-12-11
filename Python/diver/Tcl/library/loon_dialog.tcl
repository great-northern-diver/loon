
namespace eval loon {
    
    ## \brief Adaptation of tk_dialog
    #
    # \details Since the tk_dialog does not allow positioning, we
    # wrote our own version of tk_dialog
    # 
    #
    # \author Adrian Waddell
    # 
    # \param -title toplevel window title
    #
    # \param -main main text in bold
    #
    # \param -sub additional text below the -main text
    #
    # \param -bitmap If non-empty, specifies a bitmap (in a form
    # suitable for Tk_GetBitmap) to display in the top portion of the
    # dialog, to the left of the text. If this is an empty string then
    # no bitmap is displayed in the dialog.
    #
    # \param -default If this is an integer greater than or equal to
    # zero, then it gives the index of the button that is to be the
    # default button for the dialog (0 for the leftmost button, and so
    # on). If less than zero or an empty string then there will not be
    # any default button.
    #
    # \param -options There will be one button for each of these
    # arguments. Each string specifies text to display in a button, in
    # order from left to right.
    #
    # \param -parent on top of which the dialog should be centered
    
    variable Priv

    proc loon_dialog {args} {
	variable Priv
	
	if {[catch {set argDict [dict create {*}$args]}]} {
	    error "need key value pairs"
	}
	
	## creat unique toplevel window
	set tt [::loon::loon_toplevel]
	
	## set window title
	if {[dict exists $argDict -title]} {
	    wm title $tt [dict get $argDict -title]
	} else {
	    wm title ""
	}
	
	## dialog part
	frame $tt.dialog
	pack $tt\.dialog -side top -fill x  -expand 1 -pady 5
	
	## has bitmap?
	if {[dict exists $argDict -bitmap]} {
	    pack [label $tt\.dialog.icon\
		      -bitmap [dict get $argDict -bitmap]]\
		-side left -padx 10 -pady 10
	}
	
	## Main & Sub
	if {[dict exists $argDict -main]} {
	    set main [dict get $argDict -main]
	} else {
	    set main ""
	}
	if {[dict exists $argDict -sub]} {
	    set sub [dict get $argDict -sub]
	} else {
	    set sub ""
	}

	frame $tt.dialog.f
	
	set lmain [label $tt.dialog.f.main\
		       -text $main -font {bold}]
	set lsub [label $tt.dialog.f.sub\
		      -text $sub -justify left]
	
	pack $tt.dialog.f -side top -anchor c 
	
	pack $lmain $lsub -side top -anchor w
	
	frame $tt\.choice -pady 5
	pack $tt\.choice -side bottom -anchor c
	
	## Does not Work for Windows with R!
	#pack [ttk::separator $tt\.sep -orient horizontal]\
	#    -side bottom -fill x
	
	## Option buttons
	if {[dict exists $argDict -options]} {
	    set options [dict get $argDict -options]
	} else {
	    set options ""
	}

	
	set loon::Priv(button) ""
	set i 0
	foreach option $options {
	    button $tt\.choice.$i -text $option
	    pack $tt\.choice.$i -side left -padx 10
	    $tt\.choice.$i configure -command "set loon::Priv(button) $option"
	    incr i
	}

	## set default
	if {[dict exists $argDict -default]} {
	    set num [dict get $argDict -default]
	} else {
	    set num 0
	}
	
	#$tt\.choice.$num configure -relief raised -bd 2
	
	focus $tt\.choice.$num
	grab $tt
	
	#tk::SetFocusGrab $tt $tt\.choice.$num
	
	## move window
	if {[dict exist $argDict -parent]} {
	    set parent [dict get $argDict -parent]

	    update idletasks
	    set geomDialog [wm geometry $tt]
	    set geomParent [wm geometry [winfo toplevel $parent]]

	    set regpattern\
		{^([[:digit:]]+)x([[:digit:]]+)\+-{0,1}([[:digit:]]+)\+-{0,1}([[:digit:]]+)$}
	    
	    if {[regsub $regpattern $geomDialog {\1 \2 \3 \4} matchDialog] && \
		    [regsub $regpattern $geomParent {\1 \2 \3 \4} matchParent]} {
		
		set wD [lindex $matchDialog 0]
		set hD [lindex $matchDialog 1]
		
		set wP [lindex $matchParent 0]
		set hP [lindex $matchParent 1]
		set xP [lindex $matchParent 2]
		set yP [lindex $matchParent 3]
		
		#puts "wP $wP, hP $hP, wD $wD, hD $hD"
		set newX [expr {int($xP + ($wP-$wD)/2.0)}]
		set newY [expr {int($yP + ($hP-$hD)/2.0)}]
		
		wm geometry $tt "+$newX+$newY"
	    }
	}
	
	    
	# Bind Enter
	bind $tt <Return> {
	    if {[winfo class %W] in "Button TButton"} {
		%W invoke
	    }
	} 

	wm protocol $tt WM_DELETE_WINDOW {
	    
	}
	
	# choice 
	
	vwait ::loon::Priv(button)
	# Copy the result now so any <Destroy> that happens won't cause
	# trouble
	
	destroy $tt
	
	set result $::loon::Priv(button)	
    }

	
    #loon_dialog -title "aaa" -main "Your name" -sub "In letters"\
    #	-bitmap questhead -options {A B CDSSA RE} -parent .

    
    ##set reply [tk_dialog .foo "The Title" "Do you want to say yes?" \
	##	questhead 0 Yes No "I'm not sure"]

}
