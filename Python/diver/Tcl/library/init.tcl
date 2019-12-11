
namespace eval loon {
    ## Load Loon Dash Icon
    set ::env(LOON_ICON) [image create photo ::loon::loonIcon\
			      -file [file join $::env(LOON_LIBRARY)\
					 images LoonIcon.gif]]
}
