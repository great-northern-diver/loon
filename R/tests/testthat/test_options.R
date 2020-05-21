library(loon)
context("test_options")

test_that("changing options works", {
    userSettable <- l_userOptions()
    otherOptions <- setdiff(l_getOptionNames(), userSettable)
    for(optName in userSettable) {
        opt <- l_getOption(optName)
        l_setOption(optName, "fubar")
        expect_equal(l_getOption(optName), "fubar")
        l_setOption(optName, opt)
        expect_equal(l_getOption(optName), opt)
    }
    for(optName in otherOptions) {
        opt <- l_getOption(optName)
        expect_error(l_setOption(optName, "fubar"))
    }
}
)
