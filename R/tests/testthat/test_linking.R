
context("test_linking")

test_that("linking on size and selected works", {

    n <- nrow(iris)
    selected <- sample(c(T, F), size = 150, replace = TRUE)
    p1 <- l_plot(iris,
                 size = sample(c(2, 4, 8), n, replace = TRUE),
                 selected = selected,
                 linkingGroup = "iris")
    p2 <- l_plot(iris, linkingGroup = "iris")
    p3 <- l_plot3D(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length,
                   linkingGroup = "iris")
    h <- l_hist(iris, linkingGroup = "iris")
    sa <- l_serialaxes(iris, linkingGroup = "iris")

    selected <- p1['selected']
    expect_equal(p2['selected'], selected)
    expect_equal(p3['selected'], selected)
    expect_equal(h['selected'], selected)
    expect_equal(sa['selected'], selected)

    size <- p1['size']
    expect_equal(p2['size'], size)
    expect_equal(p3['size'], size)

    p3["selected"] <- sample(c(T, F), size = 150, replace = TRUE)

    expect_equal(p1['selected'], p3['selected'])
    expect_equal(p2['selected'], p3['selected'])
    expect_equal(h['selected'], p3['selected'])
    expect_equal(sa['selected'], p3['selected'])


    h["selected"] <- sample(c(T, F), size = 150, replace = TRUE)

    expect_equal(p1['selected'], h['selected'])
    expect_equal(p2['selected'], h['selected'])
    expect_equal(p3['selected'], h['selected'])
    expect_equal(sa['selected'], h['selected'])

    sa["selected"] <- sample(c(T, F), size = 150, replace = TRUE)

    expect_equal(p1['selected'], sa['selected'])
    expect_equal(p2['selected'], sa['selected'])
    expect_equal(p3['selected'], sa['selected'])
    expect_equal(h['selected'], sa['selected'])

    p2["size"] <-  sample(c(2, 4, 8), n, replace = TRUE)
    expect_equal(p1['size'], p2['size'])
    expect_equal(p3['size'], p2['size'])
})


#}
