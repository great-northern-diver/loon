
context("test_linking")

test_that("linking on size and color works", {

    n <- nrow(iris)
    p1 <- l_plot(iris,
                 color = iris$Species,
                 size = sample(c(2, 4, 8), n, replace = TRUE),
                 linkingGroup = "iris")
    p2 <- l_plot(iris, linkingGroup = "iris")
    p3 <- l_plot3D(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length,
                   linkingGroup = "iris")
    h <- l_hist(iris, linkingGroup = "iris")
    sa <- l_serialaxes(iris, linkingGroup = "iris")

    color <- p1['color']
    expect_equal(p2['color'], color)
    expect_equal(p3['color'], color)
    expect_equal(h['color'], color)
    expect_equal(sa['color'], color)

    size <- p1['size']
    expect_equal(p2['size'], size)
    expect_equal(p3['size'], size)

    p3["color"] <- sample(c("red", "green", "blue", "magenta"), n, replace = TRUE)

    expect_equal(p1['color'], p3['color'])
    expect_equal(p2['color'], p3['color'])
    expect_equal(h['color'], p3['color'])
    expect_equal(sa['color'], p3['color'])


    h["color"] <- sample(c("red", "green", "blue", "magenta"), n, replace = TRUE)

    expect_equal(p1['color'], h['color'])
    expect_equal(p2['color'], h['color'])
    expect_equal(p3['color'], h['color'])
    expect_equal(sa['color'], h['color'])

    sa["color"] <- sample(c("red", "green", "blue", "magenta"), n, replace = TRUE)

    expect_equal(p1['color'], sa['color'])
    expect_equal(p2['color'], sa['color'])
    expect_equal(p3['color'], sa['color'])
    expect_equal(h['color'], sa['color'])

    p2["size"] <-  sample(c(2, 4, 8), n, replace = TRUE)
    expect_equal(p1['size'], p2['size'])
    expect_equal(p3['size'], p2['size'])
})


#}
