## as in the rgl package

library(loon)

# options(demo.ask=FALSE)

for(demo in demo(package="loon")$results[,"Item"])
  if (!(demo %in% c("loon", "lsystem")))
    demo(demo, package="loon", character.only=TRUE)
