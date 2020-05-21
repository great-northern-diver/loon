loon_attach <- function(libname, pkgname) {
    startMsg <- paste0("loon Version ",
                         utils::packageDescription(pkg = pkgname,
                                                   field = "Version"),".",
                         "\n",
                "To learn more about loon, see l_web()."
    )
    #startMsg <- "To learn more about loon, see l_web()."
    packageStartupMessage(startMsg, appendLF = TRUE)

    #packageStartupMessage("To learn more about loon, see l_web().", appendLF = TRUE)
    invisible()
}

.onAttach <- function(libname, pkgname) {
    loon_attach(libname, pkgname)
}
