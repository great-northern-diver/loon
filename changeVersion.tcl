#! /usr/bin/env tclsh

# Change in R description
set fDE [open "R/DESCRIPTION"]

set DE [read $fDE]

if {[regexp -line {^Version: .+$} $DE matched]} {
    set oldVersion [string range $matched 9 end]    
} else {
    error "Version number not found"
}
puts "Old Version: $oldVersion"
if {[llength $argv] eq 0} {
    ## Increment current version
    set nums [split $oldVersion .]
    set new_nums [lreplace $nums end end\
		      [expr {[lindex $nums end]+1}]]
    set newVersion [join $new_nums .]
} else {
    set newVersion [lindex $argv 0]
}
puts "New Version: $newVersion"

if {![regsub -all -- $matched $DE\
	  [format "Version: %s" $newVersion]\
	  newDESCRIPTION]} {
    error "could not replace version number in DESCRIPTION"
}
if {![regsub -all -- "\nDate: .+?\n" $newDESCRIPTION\
	  [format "\nDate: %s\n"\
	       [clock format [clock seconds] -format "%Y-%m-%d"]]\
	  newDESCRIPTION]} {
    error "could not replace date in DESCRIPTION"
}



close $fDE

## Now overwrite the file
set fDE [open "R/DESCRIPTION" w]
puts -nonewline $fDE $newDESCRIPTION
close $fDE


## Now use sed to replace in place.
set pattern [format "s/loon_\%s\\.tar\\.gz/loon_%s\\.tar\\.gz/g"\
		 [regsub -all -- \\. $oldVersion \\.]\
		 [regsub -all -- \\. $newVersion \\.]]

# Change in website beta
# exec sed -i $pattern website/md/beta.md

# Change in Makefile
exec sed -i $pattern Makefile


# Change in makePkgIndex.tcl
exec cd Tcl
exec sed -i $pattern makePkgIndex.tcl
exec ./makePkgIndex.tcl
exec cd ..

puts "\n\n DIFF on affected files\n ===================== \n\n"

set diff [exec git diff --unified=0 Makefile R/DESCRIPTION\
	      Tcl/makePkgIndex.tcl]
puts [exec egrep {^(\+|-)} << $diff]
puts "\n\n"



