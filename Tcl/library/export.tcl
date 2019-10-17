
namespace eval loon {
    namespace export export
    
    proc export {widget filename {width ""} {height ""} args} {

	set resize FALSE
	if {$width ne "" && $height ne ""} {
	    set resize TRUE
	    set tt [winfo toplevel $widget]
	    set geometry [wm geometry $tt]
	    wm geometry $tt "${width}x${height}"
	    update idletasks
	}

	## widget has a canvas?
	#if {[llength [info commands ${widget}.canvas]] > 0} {
	#    set widget "${widget}.canvas"
	#}

	## Get extension
	set extn [string tolower [file extension $filename]]

	#set export_ps_font CharterBT-Roman
	set export_ps_font $::loon::Options(export_ps_font)
	
	switch -- $extn {
	    .ps -
	    .eps {
		array set ::loon::fontmap\
		    [list $::loon::Options(font-xlabel) [list $export_ps_font 12]\
			 $::loon::Options(font-ylabel) [list $export_ps_font 12]\
			 $::loon::Options(font-title) [list $export_ps_font-Bold 18]\
			 $::loon::Options(font-scales) [list $export_ps_font 12]\
			 $::loon::Options(font-text_glyph) [list $export_ps_font 12]]
		
		${widget}.canvas postscript -file $filename -fontmap ::loon::fontmap {*}$args
	    }
	    .pdf {
		## get the individual parts of path
		set dir [file dirname $filename]
		set name [file rootname [file tail $filename]]
		set current_dir [pwd]

		cd $dir
				
		## create temporary file
		set tmpfile ${name}.ps
		set i 0
		while {[file exists $tmpfile]} {
		    set tmpfile ${name}${i}.ps
		    incr i
		}

		array set ::loon::fontmap\
		    [list $::loon::Options(font-xlabel) [list $export_ps_font 12]\
			 $::loon::Options(font-ylabel) [list $export_ps_font 12]\
			 $::loon::Options(font-title) [list $export_ps_font-Bold 18]\
			 $::loon::Options(font-scales) [list $export_ps_font 12]\
			 $::loon::Options(font-text_glyph) [list $export_ps_font 12]]
		
		${widget}.canvas postscript -file $tmpfile -fontmap ::loon::fontmap {*}$args		
		set notOK [catch {exec epstopdf $tmpfile --outfile=${name}.pdf}]

		file delete $tmpfile
		cd $current_dir
		
		if {$notOK} {
		    error "need epstopdf to export as pdf"
		}
	    }
	    .jpg -
	    .jpeg -
	    .png -
	    .bmp -
	    .tiff -
	    .gif {
		if {$extn eq ".jpg"} {
		    set extn .jpeg
		}
		if {[catch {
		    set img [image create photo\
				 -format window\
				 -data $widget]
		    $img write -format\
			[string toupper [string range $extn 1 end]]\
			$filename
		} msg]} {
		    error "can not export as $extn on this system.\n$msg"
		}
		catch {image delete $img}
	    }
	    default {
		error "unknown file extension: $extn"
	    }
	}
	if {$resize} {
	    wm geometry $tt $geometry
	}
	return $filename
    }
    
    set filetypes {
	{{Portable Network Graphics} {.png}}
	{{JPEG} {.jpg} }
 	{{Postscript} {.ps}}
	{{Encapsulated Postscript} {.eps}}
	{{Portable Document Graphics} {.pdf}}
	{{Tagged Image File Format} {.tiff}}
	{{Bitmap} {.bmp}}
	{{Graphics Interchange Format} {.gif}}}
    
    # set filetypes_d [dict create\
    # 			 .png "Portable Network Graphics"\
    # 			 .jpg "JPEG"\
    # 			 .ps "Postscript"\
    # 			 .eps "Encapsulated Postscript"\
    # 			 .pdf "Portable Document Graphics"\
    # 			 .tiff "Tagged Image File Format"\
    # 			 .bmp "Bitmap"\
    # 			 .gif "Graphics Interchange Format"]
    
    proc is_tcl_package_present {pkg} {
	expr ![catch {package present $pkg}]
    }
    
    proc exportImageDialog {widget} {

	if {[::loon::is_tcl_package_present "Img"]} {
	    set types $::loon::filetypes
	} else {
	    set types {
		{{Postscript} {.ps}}
		{{Encapsulated Postscript} {.eps}}}
	}
	
	set fileName [tk_getSaveFile\
			  -filetypes $types\
			  -initialdir [pwd]\
			  -initialfile "loon_plot"\
			  -title "Export Plot As Image"\
			  -parent [winfo toplevel $widget]]
	
	if {$fileName ne ""} {
	    ::loon::export $widget $fileName
	}
    }
    
}
