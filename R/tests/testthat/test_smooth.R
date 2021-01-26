library(loon)
context("test_smooth")

test_that("test smooth", {
    p <- l_plot(iris, color = iris$Species)
    l <- l_layer_smooth(p)
    expect_equal(length(l_layer_getChildren(p, l)),
                 3)
    l_layer_expunge(p, l)
    l <- l_layer_smooth(p, group_by = "")
    expect_equal(length(l_layer_getChildren(p, l)),
                 1)
    l_layer_expunge(p, l)
    l <- l_layer_smooth(p,
                        formula = y ~ x + I(x^2), method = "lm",
                        group_by = "")
    expect_equal(length(l_layer_getChildren(p, l)),
                 1)
    l_layer_expunge(p, l)
    p['selected'] <- sample(c(TRUE, FALSE), size = 150, replace = TRUE)
    l <- l_layer_smooth(p,
                        group_by = "selected")
    expect_equal(length(l_layer_getChildren(p, l)),
                 2)
    l_layer_expunge(p, l)
    l <- l_layer_smooth(p,
                        x = p['x'][p['selected']],
                        y = p['y'][p['selected']],
                        group_by = "")
    expect_equal(length(l_layer_getChildren(p, l)),
                 1)
    l_layer_expunge(p, l)
})
