#' @title Data Scaling
#' @description Scaling the data set
#' @param data A data frame
#' @param sequence vector with variable names that are scaled.
#' If \code{NULL}, it will be set as the whole column names (all data set will be scaled).
#' @param scaling one of 'variable', 'data', 'observation' or 'none' to specify how the data is scaled. See details
#' @param displayOrder the order of the display
#' @param reserve If \code{TRUE}, return the variables not shown in \code{sequence} as well;
#' else only return the variables defined in \code{sequence}.
#' @param as.data.frame Return a matrix or a data.frame
#' @details The \code{scaling} state defines how the data is scaled. The axes
#' display 0 at one end and 1 at the other. For the following explanation
#' assume that the data is in a nxp dimensional matrix. The scaling options
#' are then
#' \tabular{ll}{
#' variable \tab per column scaling\cr
#' observation \tab per row scaling\cr
#' data \tab whole matrix scaling\cr
#' none \tab do not scale
#' }
#' @seealso \code{\link{l_serialaxes}}
#' @export
l_getScaledData <- function(data,
                            sequence = NULL,
                            scaling = c("variable", "observation", "data", "none"),
                            displayOrder = NULL,
                            reserve = FALSE,
                            as.data.frame = FALSE) {

    data <- as.data.frame(data)

    if(missing(data)) return(NULL)

    scaling <- match.arg(scaling)

    if(reserve && !is.null(sequence)) {

        colNames <- colnames(data)
        leftNames <- setdiff(colNames, sequence)

        leftData <- data[, leftNames]
        scaledData <- data[, sequence]

        d <- get_scaledData(data = scaledData,
                            sequence = sequence,
                            scaling = scaling,
                            displayOrder = displayOrder)
        rightNames <- colnames(d)

        # f return a matrix
        d <- cbind(leftData, d)
        colnames(d) <- c(leftNames, rightNames)
    } else {
        d <- get_scaledData(data = data,
                            sequence = sequence,
                            scaling = scaling,
                            displayOrder = displayOrder)
    }

    if(as.data.frame)
        as.data.frame(d, stringsAsFactors = FALSE)
    else
        as.matrix(d)
}

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

               denominator <- (maxV  - minV)
               denominator[denominator == 0] <- 1

               t(
                   (t(dat) - minV) / denominator
               )
           },
           "observation" = {

               minO <- apply(dat, 1, "min")
               maxO <- apply(dat, 1, "max")


               denominator <- (maxO - minO)
               denominator[denominator == 0] <- 1

               dat <- (dat - minO) / denominator
               dat[displayOrder, ]
           },
           "data" = {

               minD <- min(dat)
               maxD <- max(dat)

               denominator <- (maxD - minD)
               denominator[denominator == 0] <- 1

               dat <- dat[displayOrder, ]
               (dat - minD)/ denominator
           },
           "none" = {
               dat[displayOrder, ]
           })

}
