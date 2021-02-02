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

        if(is.na(aes) || is.null(aes)) {
            aesChar <- deparse(substitute(aes))
            switch(aesChar,
                   "selected" = FALSE,
                   "active" = TRUE,
                   {
                       rep(l_getOption(aesChar), n)
                   })
        } else rep(aes, n)
    }
}
