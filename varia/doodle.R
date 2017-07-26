

library(loon)

nodes <- c("sbp", "idl", "tob")
G <- completegraph(nodes)
LG <- linegraph(G)
g <- l_graph(LG)
l_navigator_add(g)

getwd()
l_export(g, "navgraph.ps")

l_export(g, "navgraph.png")


library(ggmap)
map <- get_map(location=c(lon=-80.543926, lat=43.472142), maptype = "roadmap", zoom=15)
ggmap(map)

data <- data.frame(long = runif(20, min = -80.555, max = -80.535),
                   lat = runif(20, min = 43.462, max = 43.480))
ggmap(map) + geom_point(aes(x=long, y=lat, colour="red", size=10), data=data)

p <- l_plot(data, color="red")
bb <- attr(map,"bb")
l_layer_rasterImage(p, map, 
                    xleft = bb$ll.lon, 
                    xright = bb$ur.lon, 
                    ybottom =bb$ll.lat,
                    ytop = bb$ur.lat,
                    index="end")
l_scaleto_world(p)

library(ggplot2)
p <- l_plot(diamonds)

library(loon)
p <- l_plot(iris)

library(grid)


p <- 0.5




p <- runif(4)









lp <- l_plot(p)

l_glyph_add_image(lp, li, label="bars")



