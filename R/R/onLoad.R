

.withTclImg <- FALSE        # default presumes no tkimg lib img
                            # -- should be removed -- legacy only
                            # (no reliable way to tell so checked at l_import_image() instead)

.withCimgScaler <- FALSE    # image_scale in C via the ImageScale
                            # package from https://github.com/waddella/tclImageScale
                            # Useful only if shipping a binary with faster C

.onLoad <- function(libname, pkgname) {

    libdir <- file.path(find.package(pkgname), "tcl")
    if (!dir.exists(libdir)){
        libdir <- sub("tcl$", "inst/tcl", libdir)
    }
    tcl("lappend", "auto_path", libdir)

    # Load C version of image_scale if enabled
    if (.withCimgScaler) {
        try({
            dynlib <- file.path(system.file("libs", .Platform$r_arch, package = pkgname, lib.loc = libname),
                                paste0("ImgscaleTea", .Platform$dynlib.ext))
            .Tcl(paste('load "', dynlib, '"', sep=''))
        }, silent = TRUE)
    }

    # Attempt to load the Img package (fail silently)
    try(tcl("package", "require", "Img"), silent = TRUE)

    # Load core loon Tcl
    .Tcl('package require loon')

    # Special Windows hook
    if (Sys.info()['sysname'] == "Windows") {
        .Tcl(paste0('proc ::loon::loon_toplevel {} {',
                    .Tcl.callback(.ltoplevel),
                    '; return $::loon::last_toplevel}'))
    }

    # Configuration warning control
    .Tcl('set ::loon::Options(printInConfigurationWarningMsg) ".Tcl(\'set ::loon::Options(printInConfigurationWarning) FALSE\')"')
    .Tcl('set ::loon::Options(printInConfigurationWarning) FALSE')
}

.ltoplevel <- function() {
    tt <- tktoplevel()
    tcl('set', '::loon::last_toplevel', tt$ID)
}
