
proc callstack {} {
    puts "Entered callstack:"
    set distanceToTop [info level]
    for {set i 1} {$i < $distanceToTop} {incr i} {
	set callerlevel [expr {$distanceToTop - $i}]
	puts "CALLER $callerlevel: [info level $callerlevel]"
    }

}
