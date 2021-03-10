context('test_l_compound_withParent')
library(loon)

test_that("l_compound widgets with parent", {
    iris_ <- iris[seq(149), ]
    ########### l_facet ###########
    tt <- l_toplevel()
    p <- l_plot(iris_, by = iris_$Species, parent = tt)
    # should not display
    parent <- paste0(l_subwin(tt, ""), "facet")
    tkpack(parent, fill="both", expand=TRUE)
    expect_true(is(p, "l_facet"))

    tt <- l_toplevel()
    p <- l_serialaxes(iris_[, -5], by = iris_$Species, parent = tt)
    # should not display
    parent <- paste0(l_subwin(tt, ""), "facet")
    tkpack(parent, fill="both", expand=TRUE)
    expect_true(is(p, "l_facet"))

    p <- l_plot(iris_)
    tt <- l_toplevel()
    f <- l_facet(p, by = iris_$Species, parent = tt)
    parent <- paste0(l_subwin(tt, ""), "facet")
    tkpack(parent, fill="both", expand=TRUE)
    expect_true(is(f, "l_facet"))

    s <- l_serialaxes(iris_)
    tt <- l_toplevel()
    f <- l_facet(s, by = iris_$Species, parent = tt)
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
    p <- l_pairs(iris_, parent = tt)
    parent <- paste0(l_subwin(tt, ""), "pairs")
    tkpack(parent, fill="both", expand=TRUE)
    expect_true(is(p, "l_compound"))


})
