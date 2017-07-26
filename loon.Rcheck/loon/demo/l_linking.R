
local({

    p1 <- with(trees,
               l_plot(Girth, Height, linkingGroup="trees"))

    p2 <- with(trees,
               l_plot(Height, Volume, linkingGroup="trees"))

    p3 <- with(trees,
               l_plot(Girth, Volume, color="red"))

    l_configure(p3, linkingGroup="trees", sync="push")

    readline("press the return key to continue: next disable linking of color for p3")

    l_getLinkedStates(p3)
    l_setLinkedStates(p3, c("active", "size", "selected"))

    readline("press the return key to continue: next add serialaxes plot")

    s <- l_serialaxes(trees, linkingGroup="trees")
    
    readline("press the return key to continue: next demonstrate linkingKey")

    p4 <- l_plot(1:9,1:9, color="orange",
                 linkingKey=LETTERS[2:10], linkingGroup="letters")
    g4.t <- l_glyph_add_text(p4, text=LETTERS[2:10])
    l_configure(p4, glyph=g4.t, size=10)


    p5 <- l_plot(1:4,1:4, color="green",
                 linkingKey=LETTERS[5:8], linkingGroup="letters")
    g5.t <- l_glyph_add_text(p5, text=LETTERS[5:8])
    l_configure(p5, glyph=g5.t, size=10)
    
})
