# if (requireNamespace("testthat", quietly = TRUE)) {
#
#     library(testthat)
# library(loon)
context("test_widgetPaths")

test_that("we can find unnamed loon widgets", {

    p1 <- l_plot(iris, color = iris$Species, linkingGroup = "iris")
    h1 <- l_hist(iris$Sepal.Length, linkingGroup = "iris")
    h2 <- l_hist(iris, linkingGroup = "iris")

    widgets <- l_loonWidgets()
    hists <- l_loonWidgets("hist")
    expect_true(h1 %in% hists)
})


#}
