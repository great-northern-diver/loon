loon_attach <- function(libname, pkgname) {
    startMsg <- paste0("loon Version ",
                         utils::packageDescription(pkg = pkgname,
                                                   fields = "Version"),".",
                         "\n",
                "To learn more about loon, see \n l_web() if internet browser capable, or, \n help(package = 'loon') otherwise."
    )
    packageStartupMessage(startMsg, appendLF = TRUE)

    invisible()
}

.onAttach <- function(libname, pkgname) {
    loon_attach(libname, pkgname)
}
