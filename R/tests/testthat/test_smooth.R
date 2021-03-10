library(loon)
context("test_smooth")

test_that("test smooth", {
    n <- 149
    iris_ <- iris[seq(n), ]
    p <- l_plot(iris_, color = iris_$Species)
    l <- l_layer_smooth(p)
    expect_equal(length(l_layer_getChildren(p, l)),
                 1)
    l_layer_expunge(p, l)
    l <- l_layer_smooth(p, group = iris_$Species)
    expect_equal(length(l_layer_getChildren(p, l)),
                 3)
    l_layer_expunge(p, l)
    l <- l_layer_smooth(p,
                        formula = y ~ x + I(x^2), method = "lm",
                        group = "")
    expect_equal(length(l_layer_getChildren(p, l)),
                 1)
    l_layer_expunge(p, l)
    p['selected'] <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
    l <- l_layer_smooth(p,
                        group = "selected")
    expect_equal(length(l_layer_getChildren(p, l)),
                 2)
    l_layer_expunge(p, l)
    l <- l_layer_smooth(p,
                        x = p['x'][p['selected']],
                        y = p['y'][p['selected']],
                        group = "")
    expect_equal(length(l_layer_getChildren(p, l)),
                 1)
    l_layer_expunge(p, l)

    # active
    p['active'][1:50] <- FALSE
    p['selected'] <- FALSE

    l <- l_layer_smooth(p)
    expect_equal(length(l_layer_getChildren(p, l)), 1)
    l_layer_expunge(p, l)

    l <- l_layer_smooth(p, group = iris_$Species[51:n],
                        linecolor = unique(p['color'][p['active']]))
    expect_equal(length(l_layer_getChildren(p, l)), 2)


    # test some arguments
    p <- l_plot(iris_, color = iris_$Species)
    l1 <- l_layer_smooth(p, group = c(rep(1, 100), rep(2, n - 100)),
                         level = 0.9, method = "lm", weights = ~x^5,
                         confidenceIntervalArgs = list(linecolor="blue", linewidth=2, linedash = 1),
                         predictionIntervalArgs = list(linecolor="red", linewidth=5, linedash = "")
                         )
    expect_equal(length(l_layer_getChildren(p, l1)), 2)
})
