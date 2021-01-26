aes_settings <- function(aes, n, ifNoStop = FALSE) {

    len <- length(aes)
    if (len > 1) {
        if (len != n) {
            if(ifNoStop) {
                stop("The length of ",
                     deparse(substitute(aes)),
                     " is ",
                     len,
                     " that does not match the number of points: ", n,
                     call. = FALSE)
            } else {
                rep_len(aes, n)
            }
        } else aes
    } else {

        if(is.na(aes)) {
            switch(aes,
                   "selected" = FALSE,
                   "active" = TRUE,
                   {
                       rep(l_getOption(deparse(substitute(aes))), n)
                   })
        } else rep(aes, n)
    }
}
