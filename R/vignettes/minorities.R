## ----knitr_setup, include=FALSE------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----setup, warning=FALSE, message=FALSE, error=FALSE--------------------
library(loon)

## ----minority------------------------------------------------------------
data(minority)

## ------------------------------------------------------------------------
names(minority)[1:14]

## ----minorityTables, echo=FALSE, fig.align="center"----------------------
subset <- t(minority[c("Victoria", "Ottawa", "St. John\'s"),][,1:(length(names(minority))-4)])
knitr::kable(subset)

## ---- echo=FALSE, fig.align = "center", fig.width=6.5, fig.height=3.25----
parOptions <- par(mfrow=c(1,2))
cols <- rep("steelblue", 33)
cols[6] <- "red"
cols <- adjustcolor(cols, 0.7)
with(minority,
     { 
       plot(lat, googleLat, pch=19, col=cols, asp=1)
       abline(a=0,b=1, col=adjustcolor("black", 0.5))
       text(lat[6], googleLat[6], labels=c(row.names(minority)[6]), pos=4 )
       plot(long, googleLong, pch=19, col=cols, asp=1)
       abline(a=0,b=1, col=adjustcolor("black", 0.5))
       text(long[6], googleLong[6], labels=c(row.names(minority)[6]), pos=1)
     }
     )

par(parOptions)


## ---- echo=FALSE, message=FALSE, fig.align = "center", fig.width=7, fig.height=5----
library(maps)
parOptions <- par(mfrow=c(1,2))

cols <- rep("steelblue", 33)
cols[6] <- "red"
cols <- adjustcolor(cols, 0.7)

with(minority,
     { map("world", "Canada", plot=TRUE, fill=FALSE)
       points(long, lat, pch=19, col=cols)
       points(long[6], lat[6], pch=19, col="red")
       title(main="maps source")
       text(long[6], lat[6], labels=c(row.names(minority)[6]), pos=3, offset=0.75 )
       map("world", "Canada", plot=TRUE, fill=FALSE)
       points(googleLong, googleLat, pch=19, col=cols)
       title(main="Google source")
       text(googleLong[6], googleLat[6], labels=c(row.names(minority)[6]), pos=1)
     }
     )

par(parOptions)

## ---- message=FALSE, warning=FALSE, eval=FALSE---------------------------
#   hTotPop <- with (minority,   ## Using the "with" function
#                                ## makes for easier reading
#                    l_hist(Total.population,
#                           title = "33 Canadian cities"
#                            )
#                    )
#                    ## Save the histogram structure
#                    ## to work with later from the
#                    ## command line

## ----minorityPercent-----------------------------------------------------

#  Reduce the minority data to percents of the total population
#  in each city.
#

minorityPercent <- cbind(100*minority[,c(1:10,12:14)]/minority[,"Total.population"])

#  Take the opportunity to shorten the Statistics Canada 
#  names of the minorities as well:
names(minorityPercent) <- c("% Arabic",  "% Black", "% Chinese",  
                            "% Filipino", "% Japanese",
                            "% Korean",  "% Latino", "% Multiple",
                            "% S. Asian",  "% SE Asian",  "% Other",
                            "% Minority", "% W Asian")


## ----percentVisible, eval=FALSE------------------------------------------
#  # The proportion of the city population that is a "visible minority"
#  l_hist(minorityPercent[,"% Minority"],
#         title = paste("% Minority in 33 Canadian cities"),
#         xlabel="percent of population",
#         linkingGroup="minority",
#         yshows="frequency",
#         showBinHandle=FALSE,
#         showGuides=TRUE,
#         showScales=TRUE
#  )

## ----link, eval=FALSE----------------------------------------------------
#  l_configure(hTotPop, linkingGroup = "minority", sync = "push")

## ----getTop3-------------------------------------------------------------
# Now show the same for the three minority groups
# having largest percentages (in decreasing order):
groups <- names(minorityPercent)[-c(12, 14, 15)]
largest3 <- names(sort(apply(minorityPercent[,groups ],
                             2, max),
                       decreasing = TRUE)
                  )[1:3]

## ----histTop3, eval=FALSE------------------------------------------------
#  # And now plot them
#  for (x in largest3) {
#    l_hist(minorityPercent[,x],
#           title = x,
#           xlabel="percent of population",
#           linkingGroup="minority",
#           showScales=TRUE,
#           showGuides=TRUE,
#           showStackedColors =TRUE
#           )
#  }
#  

## ----locations, eval=FALSE-----------------------------------------------
#  
#  p_map <- with(minority,
#                l_plot(long,lat,
#                       xlabel="longitude", ylabel="latitude",
#                       linkingGroup="minority",
#                       itemLabel=rownames(minority),
#                       showLabels=TRUE)
#       )
#  
#  # Add the map of Canada
#  library(maps)
#  landcol <- "grey95"
#  canada <- l_layer(p_map,
#                    map("world", "Canada",
#                        plot=FALSE, fill=TRUE),
#                    label = "Canada",
#                    color = landcol,
#                    index="end")
#  # Rescale the plot to the size of the map
#  l_scaleto_layer(p_map, canada)
#  
#  # Could also add the text of the city names to the map
#  # as glyphs
#  l_glyph_add_text(p_map,
#                   text=row.names(minority),
#                   label="city names")

## ----GreatLakes, eval=FALSE----------------------------------------------
#  
#  lakecol <- "steelblue"
#  
#  # Get the coordinates from the lakes database in the maps package.
#  # All lakes are recorded by name, so you will need to know which
#  # are in Canada.  You either know the names, or you might have to
#  # use trial and error, plotting the lakes and seeing which are inside
#  # and which are outside the country.  If the database changes,
#  # the indices below will need updating.
#  #
#  # First, we'll deal with the Great Lakes:
#  great.lakes <-  map("lakes",
#                      plot=FALSE,
#                      fill=TRUE)$names[c(7,8,27)]
#  
#  
#  # We can layer these directly
#  great.lakes.layer <-  l_layer(p_map,
#                                map("lakes", great.lakes,
#                                    plot=FALSE, fill=TRUE),
#                                label = "Great Lakes Water",
#                                color = lakecol,
#                                index="end"    # Add the layer at the end so we know
#                                               # where it is
#                                )
#  
#  # Move the lake layer up to be above the land map
#  l_layer_raise(p_map, great.lakes.layer)

## ----GreatLakesIslands, eval=FALSE---------------------------------------
#  #
#  # Great Lakes islands
#  #
#  # Unnamed islands are identified by a number added to the lake name
#  # in which they appear
#  
#  uscolor <- "grey50"
#  great.lakes.islands.US <- map("lakes",
#                                plot=FALSE,
#                                fill=TRUE)$names[c(9, 29,31,
#                                                   45:52,54,
#                                                   55, 59,
#                                                   61:63)]
#  
#  great.lakes.islands.CA <- map("lakes",
#                                plot=FALSE,
#                                fill=TRUE)$names[c(10, 28,30,
#                                                   53,56:58,
#                                                   60, 64:66)]
#  
#  
#  # Now for the islands
#  great.lakes.islands.US.layer <-l_layer(p_map,
#                                         map("lakes",
#                                             great.lakes.islands.US,
#                                             plot=FALSE, fill=TRUE),
#                                         label = "Great Lakes Islands (US)",
#                                         color= uscolor,
#                                         index="end"
#                                         )
#  
#  great.lakes.islands.CA.layer <-l_layer(p_map,
#                                         map("lakes",
#                                             great.lakes.islands.CA,
#                                             plot=FALSE, fill=TRUE),
#                                         label = "Great Lakes Islands (CA)",
#                                         color = landcol,
#                                         index="end"
#                                         )
#  
#  # We can put the two sets of islands into a group; first create the group
#  great.lakes.islands.group <- l_layer_group(p_map,
#                                            label="Great Lakes Islands",
#                                            index="end")
#  # Because it is immediately below the two sets of islands
#  # (as seen in the "Layers" inspector)
#  # we drop the islands into the group by demoting them
#  l_layer_demote(p_map, great.lakes.islands.CA.layer)
#  l_layer_demote(p_map, great.lakes.islands.US.layer)
#  
#  # Move the group layer up (twice now - once above the land, once above the lakes)
#  for (i in 1:2) l_layer_raise(p_map, great.lakes.islands.group)

## ----groupLayer, eval=FALSE----------------------------------------------
#  #
#  # Create a group layer for the great lakes
#  #
#  # First create the group layer
#  great.lakes.group <- l_layer_group(p_map,
#                                     label="Great Lakes",
#                                     index="end")
#  
#  # Move the group up to be next to the great.lakes.layer
#  l_layer_raise(p_map, great.lakes.group)
#  
#  # Demoting the great.lakes.layer puts it inside the group
#  l_layer_demote(p_map, great.lakes.layer)
#  
#  # Now the islands are above the group and can be demoted
#  # to become part of it.
#  l_layer_demote(p_map, great.lakes.islands.group)

## ----boundaryLakes, eval=FALSE-------------------------------------------
#  
#  # Now the same for the boundary lakes.
#  other.boundary.lakes <- map("lakes",
#                              plot=FALSE,
#                              fill=TRUE)$names[c(33,80)]
#  
#  other.boundary.lakes.islands <- map("lakes",
#                                      plot=FALSE,
#                                      fill=TRUE)$names[c(81)]
#  
#  # Because we know we are going to group these, we can create the group
#  # first.
#  #
#  boundary.lakes.group <- l_layer_group(p_map,
#                                        label="Boundary Lakes",
#                                        index="end")
#  
#  # We now add the layers directly to that group using the "parent"
#  # argument at creation time.
#  #
#  # Create the islands first this time.
#  other.boundary.lakes.layer <-  l_layer(p_map,
#                                         map("lakes",
#                                             other.boundary.lakes,
#                                             plot=FALSE, fill=TRUE),
#                                         label = "Other Boundary Lakes Water",
#                                         color = lakecol,
#                                         parent = boundary.lakes.group,
#                                         index="end"
#                                         )
#  
#  other.boundary.lakes.islands.layer <-  l_layer(p_map,
#                                                 map("lakes",
#                                                     other.boundary.lakes.islands,
#                                                     plot=FALSE,
#                                                     fill=TRUE),
#                                                 label = "Other Boundary Lakes Islands",
#                                                 color = uscolor,
#                                                 parent = boundary.lakes.group,
#                                                 index="end"
#                                                 )
#  
#  # Move the group layer up
#  l_layer_raise(p_map, boundary.lakes.group)

## ----interiorLakes, eval=FALSE-------------------------------------------
#  
#  
#  interior.lakes <- map("lakes",
#                        plot=FALSE,
#                        fill=TRUE)$names[c(22, 25,
#                                           68:73,
#                                           82, 85)]
#  
#  interior.lakes.islands <- map("lakes",
#                                plot=FALSE,
#                                fill=TRUE)$names[c(23:24,
#                                                   26, 74,
#                                                   83:84)]
#  # First the group layer
#  interior.lakes.group <- l_layer_group(p_map, label="Interior Lakes", index="end")
#  # Create the interior islands first
#  interior.lakes.islands.layer <- l_layer(p_map,
#                                          map("lakes",
#                                              interior.lakes.islands,
#                                              plot=FALSE, fill=TRUE),
#                                          label = "Interior lakes islands",
#                                          color = landcol,
#                                          parent = interior.lakes.group,
#                                          index="end"
#                                          )
#  
#  interior.lakes.layer <- l_layer(p_map,
#                                  map("lakes",
#                                      interior.lakes,
#                                      plot=FALSE, fill=TRUE),
#                                  label = "Interior lakes",
#                                  color = lakecol,
#                                  parent = interior.lakes.group,
#                                  index="end"
#                                  )
#  
#  # And move the group layer up
#  l_layer_raise(p_map, interior.lakes.group)

## ----selectMinorities, message=FALSE-------------------------------------
# Back to the raw data and just rename variables
minority_star <- minority[, c(
    "Southeast.Asian", "Chinese", "Japanese", "South.Asian", 
    "Visible.minority.not.included.elsewhere",
     "Black", "Multiple.visible.minority", "Filipino",
    "Arab", "Korean", "Latin.American")]

names(minority_star) <- c(
    "SE.Asian", "Chinese", "Japanese", "S.Asian", "Other",
    "Black", "Multiple", "Filipino", "Arab", "Korean", "Latino"
)

## ----selectMinoritiesTable, echo=FALSE-----------------------------------
# A look at this data
knitr::kable(head(minority_star))

## ----pairsPlot, eval=FALSE-----------------------------------------------
#  # Loon's scatterplot matrix
#  #
#  l_pairs(minority_star, linkingGroup="minority")

## ----serialAxesOverlaid, eval=FALSE--------------------------------------
#  # Now construct the serial axes display so that
#  # the cities are laid over top of one another.
#  #
#  s <- l_serialaxes(data=minority_star,
#                    axesLayout="parallel",
#                    linkingGroup = "minority",
#                    sequence=names(minority_star),
#                    showGuides=FALSE,
#                    linewidth=5,
#                    scaling = "variable",
#                    showArea=FALSE,
#                    itemLabel=rownames(minority),
#                    showItemLabels=TRUE)

## ----changeToRadial, eval=FALSE------------------------------------------
#  s["axesLayout"] <- "radial"

## ----starsGrid, eval=FALSE-----------------------------------------------
#  # Produce grid of star glyphs
#  p_stars <- with(minority,
#                  l_plot(rank(long), rank(long),
#                         title = "",
#                         showScales = FALSE,
#                         xlabel = "",
#                         ylabel = "",
#                         showLabels=FALSE, size=3,
#                         linkingGroup = "minority",
#                         itemLabel=rownames(minority),
#                         showItemLabels=TRUE))
#  
#  gstars <-l_glyph_add_serialaxes(p_stars,
#                                  axesLayout = "radial",
#                                  data=minority_star,
#                                  sequence = names(minority_star),
#                                  scaling = 'variable',
#                                  showArea=TRUE)
#  p_stars['glyph'] <- gstars

## ----moveToGrid, eval=FALSE----------------------------------------------
#  #
#  l_move_grid(p_stars, which='all')
#  # swapAxes neccessary to arrange west-east in the grid (columns)
#  p_stars['swapAxes'] <- TRUE
#  l_zoom(p_stars, .9)
#  #

## ----westEastCols, eval=FALSE--------------------------------------------
#  library(scales)
#  west_east_cols <- (col_numeric(c("firebrick",
#                                   "orange", "sienna",
#                                   "darkgreen", "steelblue",
#                                   "purple"),
#                                 domain = NULL)
#                     )(minority$long)
#  
#  # Just need to color the glyphs on one of the plots
#  # since they are all linked.
#  s['color'] <- west_east_cols

## ----starMap, eval=FALSE-------------------------------------------------
#  # Add some glyphs to the map,
#  # scale values within variates
#  so <- l_glyph_add_serialaxes(p_map, data=minority_star,
#                               sequence = names(minority_star),
#                               scaling = 'variable',
#                               showArea=TRUE,
#                               label="stars")
#  p_map['glyph'] <- so

## ----observationScale, eval=FALSE----------------------------------------
#  s["scaling"] <- "observation"
#  gstars["scaling"] <- "observation"
#  so["scaling"] <- "observation"
#  
#  # Double the size of the glyphs on the map.
#  p_map["size"] <- 2 * p_map["size"]
#  

## ----moveGrid, eval=FALSE------------------------------------------------
#  l_move_grid(p_stars, which='all')

## ----PairViz, message=FALSE----------------------------------------------
# Axis ordering methods can be found
# in the pairwise coordinate visualization package
# called PairViz
library(PairViz)

## ----distances-----------------------------------------------------------
# First transform minority_star to within city percentages.
minority_star_percent <- 100*minority_star/minority[,"Total.population"]

# Each variate is now treated as a position in the
# space of the cities.  We calculate the distances
# between the variates.
# 
VarDistance <- dist(t(minority_star_percent))

## ------------------------------------------------------------------------
# We will use these distances in a travelling
# salesman problem that chooses an order to have
# minimal distances. 
# The appropriate function from PairViz is order_tsp(...)
# 
# Placing minorities that are close to each other
# (in the above distance) beside each other should
# result in somewhat more convex shaped glyphs.
# 
o_near <- order_tsp(VarDistance)
# The order of minorities:
names(minority_star_percent)[o_near]


# Alternatively, placing distant variates beside each
# other should produce less convex shaped glyphs.
# 
o_far <- order_tsp(-VarDistance)
# The order of minorities:
names(minority_star_percent)[o_far]

## ----PairVizOverlaid, eval=FALSE-----------------------------------------
#  # Now construct the serial axes display so that
#  # the cities are laid over top of one another.
#  #
#  s_close <- l_serialaxes(data=minority_star,
#                          linkingGroup = "minority",
#                          sequence=names(minority_star)[o_near],
#                          showGuides=FALSE,
#                          linewidth=5,
#                          scaling = "observation",
#                          showArea=FALSE,
#                          title="Nearest minority order",
#                          itemLabel=rownames(minority),
#                          showItemLabels=TRUE)
#  
#  s_far <- l_serialaxes(data=minority_star,
#                        linkingGroup = "minority",
#                        sequence=names(minority_star)[o_far],
#                        showGuides=FALSE,
#                        linewidth=5,
#                        scaling = "observation",
#                        showArea=FALSE,
#                        title="Farthest minority order",
#                        itemLabel=rownames(minority),
#                        showItemLabels=TRUE)

## ----PairsVizGrid, eval=FALSE--------------------------------------------
#  
#  gstars_near <-l_glyph_add_serialaxes(p_stars,
#                                  data=minority_star,
#                                  sequence = names(minority_star)[o_near],
#                                  scaling = 'observation',
#                                  label = "Nearest",
#                                  showArea=TRUE)
#  p_stars['glyph'] <- gstars_near
#  
#  gstars_far <-l_glyph_add_serialaxes(p_stars,
#                                  data=minority_star,
#                                  sequence = names(minority_star)[o_far],
#                                  scaling = 'observation',
#                                  label = "Farthest",
#                                  showArea=TRUE)
#  p_stars['glyph'] <- gstars_far
#  

## ----PairVizstarMap, eval=FALSE------------------------------------------
#  # Add  glyphs to the map
#  so_near <- l_glyph_add_serialaxes(p_map, data=minority_star,
#                                    sequence = names(minority_star)[o_near],
#                                    scaling = 'observation',
#                                    showArea=TRUE,
#                                    label="Nearest")
#  p_map['glyph'] <- so_near
#  
#  so_far <- l_glyph_add_serialaxes(p_map, data=minority_star,
#                                   sequence = names(minority_star)[o_far],
#                                   scaling = 'observation',
#                                   showArea=TRUE,
#                                   label="Farthest")
#  p_map['glyph'] <- so_near

## ----Eulerians-----------------------------------------------------------
o_greedy <- eulerian(VarDistance)
o_balanced <- weighted_hpaths(VarDistance, matrix=FALSE)

