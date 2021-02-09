library(loon)
context("test_zeroOneDim_data")

test_that("test_zero_dim_data", {
    p <- l_plot()
    expect_true(is(p, "l_plot"))

    h <- l_hist()
    expect_true(is(h, "l_hist"))

    g <- l_graph()
    expect_true(is(g, "l_graph"))

    p3D <- l_plot3D()
    expect_true(is(p3D, "l_plot3D"))

    s <- l_serialaxes()
    expect_true(is(s, "l_serialaxes"))
})

test_that("test_one_dim_data", {
    p <- l_plot(1)
    expect_true(p['x'] == 1)

    h <- l_hist(1)
    expect_true(h['x'] == 1)

    g <- l_graph(1)
    expect_true(g['nodes'] == "1")

    p3D <- l_plot3D(1, 1, 1)
    expect_true(p3D['x'] == 1)

    s <- l_serialaxes(as.data.frame(matrix(1:10, nrow = 1)))
    expect_true(all(unlist(s['data']) == as.character(1:10)))
})
