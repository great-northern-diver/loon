library(loon)
context("test_userSettableOptions")

test_that("user settable options work", {
    userSettable <- l_userOptions()
    colorOpts <- c("color", "select-color",
                   "brush_color", "brush_color_handle",
                   "background", "foreground",
                   "guidesBackground", "guidelines")
    expect_setequal(intersect(colorOpts, userSettable),
                    colorOpts)

    for (colorOpt in colorOpts) {
        oldCol <- l_getOption(colorOpt)
        newCol <- "steelblue"
        l_setOption(colorOpt, newCol)
        expect_equal(l_getOption(colorOpt), newCol)
        l_setOption(colorOpt, oldCol)
        expect_equal(l_getOption(colorOpt), oldCol)
    }

    default_glyph <- l_getOption("glyph")
    new_glyph <- "otriangle"
    l_setOption("glyph", new_glyph)
    expect_equal(l_getOption("glyph"), new_glyph)
    l_setOption("glyph", default_glyph)

})


