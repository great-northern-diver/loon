# This function is deprecated now and will be removed later
color.id <- function (col) {
    vapply(col, function(color) {
        if (!grepl("#", color))
            return(color)
        tryCatch(expr = {
            color <- as_hex6color(color)
            c2 <- grDevices::col2rgb(color)
            coltab <- grDevices::col2rgb(colors())
            cdist <- apply(coltab, 2, function(z) sum((z - c2)^2))
            colors()[which(cdist == min(cdist))][1]
        }, error = function(e) {
            color
        })
    }, character(1))
}
