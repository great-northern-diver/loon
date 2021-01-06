context('test_l_compound_withParent')
library(loon)

test_that("l_compound widgets with parent", {
    ########### l_facet ###########
    tt <- l_toplevel()
    p <- l_plot(iris, by = iris$Species, parent = tt)
    # should not display
    parent <- paste0(l_subwin(tt, ""), "facet")
    tkpack(parent, fill="both", expand=TRUE)
    expect_true(is(p, "l_facet"))

    tt <- l_toplevel()
    p <- l_serialaxes(iris[, -5], by = iris$Species, parent = tt)
    # should not display
    parent <- paste0(l_subwin(tt, ""), "facet")
    tkpack(parent, fill="both", expand=TRUE)
    expect_true(is(p, "l_facet"))

    p <- l_plot(iris)
    tt <- l_toplevel()
    f <- l_facet(p, by = iris$Species, parent = tt)
    parent <- paste0(l_subwin(tt, ""), "facet")
    tkpack(parent, fill="both", expand=TRUE)
    expect_true(is(f, "l_facet"))

    s <- l_serialaxes(iris)
    tt <- l_toplevel()
    f <- l_facet(s, by = iris$Species, parent = tt)
    parent <- paste0(l_subwin(tt, ""), "facet")
    tkpack(parent, fill="both", expand=TRUE)
    expect_true(is(f, "l_facet"))

    ########### l_ts ###########
    tt <- l_toplevel()
    m <- decompose(co2)
    p <- l_plot(m, parent = tt)
    parent <- paste0(l_subwin(tt, ""), "ts")
    tkpack(parent, fill="both", expand=TRUE)
    expect_true(is(p, "l_compound"))

    ########### l_pairs ###########
    tt <- l_toplevel()
    p <- l_pairs(iris, parent = tt)
    parent <- paste0(l_subwin(tt, ""), "pairs")
    tkpack(parent, fill="both", expand=TRUE)
    expect_true(is(p, "l_compound"))


})
