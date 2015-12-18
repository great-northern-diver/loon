
package require loon
package require Img

namespace import loon::*

## set up data
set olive $::loon::data::olive
# extract variables
dict for {name value} $loon::data::olive {set $name $value}
# create oliveAcids data
set oliveAcids [dict filter $loon::data::olive script {key value} {
    return -level 0 [expr {$key ni {Area Region}}]
}]; puts "filter data"


puts "this demo is meant to be run with tkcon: press return to start"; gets stdin


set p [plot -x $arachidic -y $oleic -color $Area]

## Text
puts "press the return key to continue: next are text glyphs"; gets stdin

set gt [$p glyph add text -text $Area]
$p configure -glyph $gt

## Images
puts "press the return key to continue: next are image glyphs"; gets stdin

set files [glob -directory [file join images olive] -- *.png]

set imgs {}
set img_names {}
foreach file $files {
    lappend imgs [image create photo -file $file]
    lappend img_names [file rootname [file tail $file]]
}
set area {}
foreach i_area $Area {
    lappend area [regsub "^.*-" $i_area ""]
}
set images {}
foreach a $area {
    lappend images [lindex $imgs [lsearch $img_names Liguria]]
}

set gi [$p glyph add image -images $images]

$p configure -glyph $gi


puts "press the return key to continue: next reuse single image"; gets stdin

$p glyph with $gi configure -images [lindex $imgs 0]

## Serialaxes
puts "press the return key to continue: next star glyphs";  gets stdin

set sa [$p glyph add serialaxes -data $oliveAcids]
$p configure -glyph $sa

puts  "press the return key to continue: next configure stars"; gets stdin

$p glyph with $sa configure -showEnclosing FALSE -linewidth 2

$p glyph with $sa configure -showArea TRUE


puts "press the return key to continue: next stack all Umbria glyphs"; gets stdin

$p configure -selected [lmap x $Area {if {$x eq "Umbria"} {format TRUE} else {format FALSE}}]


$p move valign selected
$p move halign selected
$p configure -size 40 -which selected

$p scaleto selected

$p glyph with $sa configure -showEnclosing TRUE -bboxColor steelblue\
    -showArea FALSE

$p configure -selected FALSE

puts "press the return key to continue: next parallel coordinates"; gets stdin


$p move reset all

$p configure -size 4

$p scaleto world

$p glyph with $sa configure -axesLayout parallel

puts "press the return key to continue: next change the sequence"; gets stdin

$p glyph with $sa configure -sequence\
    [list palmitic palmitoleic stearic oleic linoleic\
	 linolenic arachidic eicosenoic palmitoleic oleic\
	 palmitic linolenic stearic eicosenoic linoleic\
	 arachidic oleic linolenic palmitoleic eicosenoic\
	 palmitic arachidic stearic linoleic linolenic\
	 eicosenoic oleic arachidic palmitoleic linoleic\
	 palmitic stearic]


puts "press the return key to continue: next show axes"; gets stdin


$p glyph with $sa configure -showAxes TRUE
    
puts "press the return key to continue: switch back to star glyphs"; gets stdin
    
$p glyph with $sa configure -axesLayout radial\
    -showAxes FALSE -showEnclosing FALSE
    

## Mix glyphs
puts "press the return key to continue: mix between different glyphs"; gets stdin


set glyphs [list circle square triangle diamond\
		ocircle osquare otriangle odiamond\
		$gt $gi $sa]
set nglyphs [llength $glyphs]

set g {}
for {set i 0} {$i < [llength $Area]} {incr i} {
    lappend g [lindex $glyphs [expr {int(rand()*$nglyphs)}]]
}
set g

$p configure -glyph $g


puts "end of demo"