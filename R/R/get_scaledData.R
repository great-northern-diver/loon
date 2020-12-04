#' @title scale data
#' @description It is mainly used in serial axes
#' @param data A data frame
#' @param sequence vector with variable names that defines the axes sequence.
#' If \code{NULL}, it will be set as the column names automatically.
#' @param scaling one of 'variable', 'data', 'observation' or 'none' to specify how the data is scaled.
#' @param displayOrder the order of the display
#' @param reserve If \code{TRUE}, return the variables not shown in \code{sequence} as well;
#' else only return the variables defined in \code{sequence}.
#' @param as.data.frame Return a matrix or a data.frame
#' @export
get_scaledData <- function(data,
                           sequence = NULL,
                           scaling = c("variable", "observation", "data", "none"),
                           displayOrder = NULL) {

    data <- as.data.frame(data)

    # data is the original data set
    # since "variable" scaling is based on the original data
    if(missing(data) || is.null(data)) return(NULL)

    if(is.null(displayOrder)) displayOrder <- seq(nrow(data))

    if(!is.null(sequence)) {

        col_name <- make.names(colnames(data))
        # sequence names may involve invalid chars
        # such as `(`, `)`, ` ` space, etc.
        # call function `make.names` can remove all these chars to match data column names
        sequence <- make.names(sequence)

        if(!all(sequence %in% col_name)) {
            warning("unknown variable names in sequence")
            sequence <- intersect(sequence, col_name)
        }
        data <-  data[, sequence]
    }

    scaling <- match.arg(scaling)

    is_char <- FALSE
    is_factor <- FALSE
    is_logical <- FALSE

    dat <- sapply(data,
                  function(x) {
                      if(is.numeric(x)) x
                      else if(is.character(x)) {
                          is_char <<- TRUE
                          as.numeric(as.factor(x))
                      } else if (is.factor(x)) {
                          is_factor <<- TRUE
                          as.numeric(x)
                      } else if(is.logical(x)) {
                          is_logical <<- TRUE
                          as.numeric(x)
                      } else stop("unknown data structure")
                  })
    # give warning once
    if(is_char || is_factor || is_logical)
        warning("No numerical columns exist", call. = FALSE)

    if(length(displayOrder) == 1) {
        dat <- setNames(as.data.frame(matrix(dat, nrow = 1)), names(dat))
        if(scaling == "variable") {
            warning("Only one observation in serialAxesData, 'scaling' will be set as 'data' by default")
            scaling <- 'data'
        }
    }

    switch(scaling,
           "variable" = {
               minV <- apply(dat, 2, "min")
               maxV <- apply(dat, 2, "max")
               dat <- dat[displayOrder, ]
               t(
                   (t(dat) - minV) / (maxV  - minV)
               )
           },
           "observation" = {
               minO <- apply(dat, 1, "min")
               maxO <- apply(dat, 1, "max")
               dat <- (dat - minO) / (maxO - minO)
               dat[displayOrder, ]
           },
           "data" = {
               minD <- min(dat)
               maxD <- max(dat)
               dat <- dat[displayOrder, ]
               (dat - minD)/ (maxD - minD)
           },
           "none" = {
               dat[displayOrder, ]
           })

}
