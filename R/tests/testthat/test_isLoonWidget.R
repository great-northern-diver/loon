library(loon)
context("test_isLoonWidget")

pdf(NULL)
test_that("test objects is a loon widget", {
    expect_false(loon::l_isLoonWidget(NULL))
    expect_false(loon::l_isLoonWidget(numeric(0L)))
    expect_false(loon::l_isLoonWidget(NA))
    expect_false(loon::l_isLoonWidget(NaN))
    p <- l_plot()
    expect_true(loon::l_isLoonWidget(p))
    # An `l_compound` object is not a `loon` widget
    pf <- l_plot(iris, by = iris$Species)
    expect_false(loon::l_isLoonWidget(pf))
})
