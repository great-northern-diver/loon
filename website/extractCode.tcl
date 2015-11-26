#! /usr/bin/env tclsh


## extract which language?
if {[llength $argv] eq 0} {
    set language R
} else {
    set language [lindex $argv 0]
}

if {$language ni {R Tcl}} {
    puts "language \"$language\" not known. R used instead."
    set language R
}


## Output
switch -- $language {
    R {
	set OUT [open "R_code.R" w]
    }
    Tcl {
	set OUT [open "Tcl_code.tcl" w]
    }
    default {
	error "language $language not known"
    }    
}


## learn files
set files [list intro states linking layer bind\
	       display_hist display_plot display_serialaxes\
	       display_pairs display_graph display_inspectors\
	       display_inspectors]




set inChunk FALSE
set notRun FALSE

set chunkMode ""

set re {^(\t[\t\s]{4,}|\t{2,})[^*]}

foreach file $files {
    set IN [open [file join md generated [format "learn_%s_%s.md" $language $file]]]

    puts $OUT "\n\n## Section: $file"
    
    while {[gets $IN line] >= 0} {

	if {$inChunk} {
	    set terminate FALSE
	    switch -- $chunkMode {
		tilde {
		    if {[string range $line 0 2] eq "~~~"} {
			set terminate TRUE
		    }
		}
		spaces {
		    if {![regexp $re $line]} {
			set terminate TRUE
		    }
		}
		default {
		    puts "wrong chunkMode: $chunkMode"
		}
	    } 
	    
	    if {$terminate} {
		set inChunk FALSE
		puts $OUT ""
	    } else {
		if {!$notRun} {
		    puts $OUT $line
		}
	    }	    
	} else {
	    
	    if {[string range $line 0 2] eq "~~~"} {
		set inChunk TRUE
		set chunkMode tilde
		if {[regexp {\.notrun} $line]} {
		    set notRun TRUE 
		} else {
		    set notRun FALSE
		}
		
	    } elseif {[regexp $re $line]} {
		set inChunk TRUE
		set chunkMode spaces
		puts $OUT $line
	    }
	}
    }
    close $IN
}

close $OUT
