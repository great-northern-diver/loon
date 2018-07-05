
local({

    p1 <- with(trees,
               l_plot(Girth, Height, linkingGroup="trees",
                      title = "p1"))

    p2 <- with(trees,
               l_plot(Height, Volume, linkingGroup="trees",
                      title = "p2"))

    cat(paste("\n\n Plots p1 and p2 are linked via the linkingGroup: \"trees\" \n",
              "\t Select points and change their, size, color, and shape. \n \n"
    )
    )
    
    
    cat(paste("\t Next will be a plot, \"p3\", with random glyphs, colors, and sizes, \n", 
              "\t the same linkingGroup (\"trees\"), but \"p3\"  will \n",
              "\t sync with p1 and p2 by pushing its information to the group.\n")
    )
    readline("\t Press the return key to continue: ")
    
    n <- nrow(trees)
    
    p3 <- with(trees,
               l_plot(Girth, Volume, 
                      color = sample(palette(), n, replace = TRUE),
                      size = sample(1:20, n, replace = TRUE),
                      glyph = sample(l_primitiveGlyphs(), n, replace = TRUE),
                      title = "p3"))

    l_configure(p3, linkingGroup="trees", sync="push")

    cat("\n\n Next we disable the linking of color for p3. \n \n")
    readline("\t Press the return key to continue: \n")

    l_getLinkedStates(p3)
    l_setLinkedStates(p3, c("active", "size", "selected"))
    
    cat("\n\n Next a serialaxes plot will be added to the linkingGroup. \n \n")
    readline("\t Press the return key to continue: \n")

    s <- l_serialaxes(trees, linkingGroup="trees")
    
    cat(paste("\n\n The other important argument affecting linking is the linkingKey. \n",
              "\t Plots participating in the same linkingGroup, link points that have the same linkingKey. \n",
              "\t Next two new plots with different numbers of points will be linked.\n",
              "\t Points having the same letter have the same linkingKey. \n \n")
    )
    readline("\t Press the return key to continue: \n")
    
    p4 <- l_plot(1:9,1:9, color="orange",
                 linkingKey=LETTERS[2:10], linkingGroup="letters")
    g4.t <- l_glyph_add_text(p4, text=LETTERS[2:10])
    l_configure(p4, glyph=g4.t, size=10)


    p5 <- l_plot(1:4,1:4, color="green",
                 linkingKey=LETTERS[5:8], linkingGroup="letters")
    g5.t <- l_glyph_add_text(p5, text=LETTERS[5:8])
    l_configure(p5, glyph=g5.t, size=10)
    
}
)
