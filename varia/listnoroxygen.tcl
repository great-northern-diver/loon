
# from http://wiki.tcl.tk/1474
proc glob-r {pattern {dir .}} {
    set res {}
    foreach i [lsort [glob -nocomplain -dir $dir $pattern]] {
        if {[file type $i] eq "directory"} {
            lappend res [glob-r $i]
        } else {
            lappend res $i
        }
    }
    return $res
}

set files [glob-r *.R "~/Desktop/loon/R/R"]

# set file [lindex $files 20]

set fun ""
set hasTitle FALSE
set hasExport FALSE


puts ""
foreach file $files {
    set fp [open $file r]

    while { [gets $fp line] >= 0} {

	if {[regexp {@(title|temp)} $line]} {
	    set hasTitle TRUE
	} elseif {[regexp {@export} $line]} {
	    set hasExport TRUE
	} elseif {[regexp {^\s*([0-9a-zA-Z_\.]+)\s*<-\s*function\(} $line all fun]} {
	    if {$hasTitle && $hasExport} {
		# puts "function $fun has roxgen docummented."
	    } elseif {$hasExport} {
		puts "$fun"
	    } else {
		# puts "function $fun is not not exported"
	    }
	    set hasTitle FALSE
	    set hasExport FALSE
	}
	# puts "$line"
    }    
    close $fp
}
