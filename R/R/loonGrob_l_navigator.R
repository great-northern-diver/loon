
#' @rdname loonGrob
#'
#' @export

loonGrob.l_navigator <- function(target, name = NULL, gp = NULL, vp = NULL){
    # Probably never used
    widget <- target
    color <- as_hex6color(widget['color'])
    label <- widget['label']

    vp <- vpStack(
        plotViewport(margins = c(4.1, 5.1, 2.1, 2.1), name = "plotViewport"),
        dataViewport(xscale = c(0,1), yscale = c(0,1), name = "dataViewport")
    )

    gList(
        pointsGrob(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                   gp = gpar(fill = color, cex = 3.5),
                   pch = 21, vp = vp,
                   name = "navigator"
        ),
        condGrob(
            test = length(label) != 0,
            grobFun = textGrob,
            name = "navigator label",
            x = unit(0.5, "npc"),
            y = unit(0.5, "npc"),
            label = paste(label, collapse = " "),
            gp = gpar(fill = l_getOption("foreground"), fontsize = 9),
            vp = vp
        )
    )
}
