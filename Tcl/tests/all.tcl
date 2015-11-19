package require Tcl 8.6
package require tcltest 2.2

lappend auto_path [file dirname [file dirname [file normalize [info script]]]]
package require loon

::tcltest::configure -testdir \
    [file dirname [file normalize [info script]]]

eval ::tcltest::configure $argv
::tcltest::runAllTests

