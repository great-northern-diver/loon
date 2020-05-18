loon_attach <- function() {
    packageStartupMessage("To learn more about loon, see l_web().", appendLF = TRUE)
    invisible()
}

.onAttach <- function(...) {
    loon_attach()
}
