# if (requireNamespace("testthat", quietly = TRUE)) {
#
#     library(testthat)
    library(loon)
    context("test_l_pairs")

    test_that("l_pairs logicals work", {
        data <- iris[seq(149), -5]
        nVars <- length(data)
        p <- l_pairs(data,
                     histArgs = list(showStackedColors = FALSE,
                                     origin = 2,
                                     binwidth = 2,
                                     color = "pink",
                                     showBinHandle = TRUE),
                     showHistograms = TRUE)
        plots <- l_getPlots(p)
        is.hist <- function(plot) {"l_hist" %in% class(plot)}
        is.plot <- function(plot) {"l_plot" %in% class(plot)}
        nscatterplots <- sum(sapply(plots, FUN = is.plot))
        nhists <- sum(sapply(plots, FUN = is.hist))
        expect_equal(nscatterplots, choose(nVars, 2) )
        expect_equal(nhists,  2 * (nVars - 1))

        someHist <- Find(is.hist, plots )
        expect_true(someHist['showBinHandle'])
        expect_true(someHist['binwidth'] == 2)
        expect_true(someHist['origin'] == 2)
        expect_true(!someHist['showStackedColors'])

        psa <- l_pairs(data,
                     showSerialAxes = TRUE,
                     serialAxesArgs = list(
                         scaling = "observation",
                         axesLayout = "parallel",
                         showAxes = FALSE
                     ))
        is.sa <- function(plot) {"l_serialaxes" %in% class(plot)}
        saplots <- l_getPlots(psa)
        sa <- Find(is.sa, saplots )
        expect_true(sa['scaling'] == "observation")
        expect_true(sa['axesLayout'] == "parallel")
        expect_true(!sa['showAxes'])
    })


#}
