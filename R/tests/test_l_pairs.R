if (requireNamespace("testthat", quietly = TRUE)) {

    library(testthat)
    library(loon)
    context("test_l_pairs")

    test_that("example works", {
        p <- l_pairs(iris[, -5],
                     histArgs = list(showStackedColors = FALSE,
                                     origin = 2,
                                     binwidth = 2,
                                     color = "pink",
                                     showBinHandle = TRUE),
                     showHistograms = TRUE)
        expect_true(p[[10]]['showBinHandle'])
        expect_true(p[[10]]['binwidth'] == 2)
        expect_true(p[[10]]['origin'] == 2)
        expect_true(!p[[10]]['showStackedColors'])

        p <- l_pairs(iris[, -5],
                     showSerialAxes = TRUE,
                     serialAxesArgs = list(
                         scaling = "observation",
                         axesLayout = "parallel",
                         showAxes = FALSE,
                         color = "red"
                     ))
        expect_true(p[[length(p)]]['scaling'] == "observation")
        expect_true(p[[length(p)]]['axesLayout'] == "parallel")
        expect_true(!p[[length(p)]]['showAxes'])
    })


}
