library(loon)
context("test_andrews")

test_that("test andrews glyphs", {
    sam <- sample(1:149, 50)
    p <- l_plot(iris[sam, ])
    gs <- l_glyph_add_serialaxes(p, data=iris[sam, -5],
                                 andrews = TRUE)
    p['glyph'] <- gs
    expect_true(gs['andrews'])
    # test layout sequence
    gs['sequence'] <- c(colnames(iris[1:149, -5]), "Sepal.Length")
    expect_equal(gs['sequence'], c(colnames(iris[1:149, -5]), "Sepal.Length"))

    # test scaling
    gs['scaling'] <- "observation"
    expect_equal(gs['scaling'], "observation")

    # test showAxes
    gs['showAxes'] <- TRUE
    expect_true(gs['showAxes'])

    # test showEnclosing
    gs['showEnclosing'] <- TRUE
    expect_true(gs['showEnclosing'])

    # test showArea
    gs['showArea'] <- TRUE
    expect_true(gs['showArea'])

    # test axesLayout parallel
    # we need to test layout sequence, scaling,
    # showAxes, showEnclosing and showArea again
    # that is because, they use very different tcl code
    gs['axesLayout'] <- "parallel"
    expect_equal(gs['axesLayout'], "parallel")

    # test layout sequence
    gs['sequence'] <- c(colnames(iris[1:149, -5]))
    expect_equal(gs['sequence'], c(colnames(iris[1:149, -5])))

    # test scaling
    gs['scaling'] <- "data"
    expect_equal(gs['scaling'], "data")

    # test showAxes
    gs['showAxes'] <- FALSE
    expect_true(!gs['showAxes'])

    # test showEnclosing
    gs['showEnclosing'] <- FALSE
    expect_true(gs['andrews'])

    # test showArea
    gs['showArea'] <- FALSE
    expect_true(gs['andrews'])
})


test_that("test andrews plot", {
    set.seed(12345)
    sam <- sample(1:149, 50)
    s <- l_serialaxes(iris[sam, ], andrews = TRUE)
    expect_true(s['andrews'])

    s['color'] <- iris[sam, "Species"]
    expect_true(length(unique(s['color'])) == 3)

    s['showGuides'] <- FALSE
    expect_false(s['showGuides'])

    s['showAxes'] <- FALSE
    expect_false(s['showAxes'])

    s['showAxesLabels'] <- FALSE
    expect_false(s['showAxesLabels'])

    s['scaling'] <- "observation"
    expect_equal(s['scaling'], "observation")

    s['axesLayout'] <- "parallel"
    expect_equal(s['axesLayout'], "parallel")
})
