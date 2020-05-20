# options(demo.ask=FALSE)

# is_windows <- Sys.info()['sysname'] == "Windows"
#
# for(demo in demo(package="loon")$results[,"Item"]) {
#     if (!(demo %in% c("loon", "lsystem"))) {
#         # on windows all the image resizing seem to use up too much memory
#         # when all the demos are run sequentially
#         if (!(is_windows && grepl("^l_ng_", demo))) {
#             demo(demo, package="loon", character.only=TRUE)
#         }
#     }
# }
