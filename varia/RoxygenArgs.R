
# todo:
# datasets.R
#


# documment as much programmatically as possible
library(loon)
args <- Map(
    formalArgs,
    Filter(
        is.function,
        Map(function(x)get(x, pos = "package:loon"), ls("package:loon"))
    )
)


## Cat Roxygen Template
args2 <- Map(function(args, fun) {
    list(fname = fun, args = args)
}, args, names(args))



rt <- function(fname) {
    if (!(fname %in% names(args)))
        stop(paste("fname ", fname, "not known"))
    cat(paste(c(
        c(
        "#' @title ",
        "#' ",  
        "#' @description ",
        "#' ",  
        "#' @details ",
        "#' "),
        unlist(Map(function(x)paste0("#' @param ", x, " "), args[fname])),
        c(
            "#' ",
            "#' @return ",
            ""
        )
    ), collapse = "\n"))
}

rt("l_cget")


Map(function(x) {
    cat(x$fname)
    
}, args2)



## data frame
X <- Reduce(
    rbind,
    Map(
        function(x,y)data.frame(fn=rep(y, length(x)), arg=x, stringsAsFactors=FALSE),
        args,
        names(args)
    )
)

uargs <- unique(X$arg)
length(uargs)

Filter(function(x)"tclobj"%in%as.character(x), args)


## Read all R files and list parameters






## create templates

names(args)
?table
sort(tapply(X$arg,X$arg,length))


fnWithArg("ids")

argdef <- list(
    general = list(
        widget = "widget path name as a string or plot hanlde returned by a plot",
        target = "",
        "..." = "",
        parent = "", #used for parent widget path and parent layer
        id = "",  #layer
        x = "x coordinates",
        y = "y coordinates",
        label = "",
        layer = "",
        index = "numeric position among its siblings, stats with 0, the string 'end' is valid too",
        color = "",
        linecolor = "",
        linewidth = "",
        ids = "binding ids",
        data = "",
        which = "",
        event = "",
        callback = "",
        xlim = "",
        ylim = "",
        #
        graph = "",
        nodes = "",
        navigator = "",
        from = "",
        to = "",
        loongraph = "",
        isDirected = "",
        separator = "",
        fun = "",
        value = "",
        tags = "",
        state = "",
        group = "",
        type = "",
        filename = "",
        width = "",
        height = "",
        asloongraph = "",
        images = "",
        ymin = "",
        ymax = "",
        showArea = "",
        sequence = "",
        scaling = "",
        axesLayout = "",
        showAxes = "",
        axesColor = "",
        showEnclosing = "",
        bboxColor = "",
        text = "",
        activewidget = "",
        browser = "",
        colors = "",
        origin = "",
        binwidth = "",
        array = "",
        img_in_row = "",
        invert = "",
        rotate = "",
        paths = "",
        tclimages = "",
        states = "",
        alphaX = "",
        alphaY = "",
        z = "",
        nlevels = "",
        levels = "",
        asSingleLayer = "",
        zlim = "",
        col = "",
        breaks = "",
        oldstyle = "",
        useRaster = "",
        dash = "",
        map = "",
        size = "",
        image = "",
        xleft = "",
        ybottom = "",
        xright = "",
        ytop = "",
        angle = "",
        interpolate = "",
        factor = "",
        amount = "",
        proportion = "",
        path = "",
        tclobj = "",
        transform = "",
        measures = "",
        aspect = "",
        palette = "",
        chroma = "",
        luminance = "",
        hue_start = "",
        cast = "",
        n = "",
        scagnostics = ""
    ),
    l_graphswitch_reorder = list(
        ids = ""
    )
)

for (a in uargs[which(is.na(match(uargs, names(argdef$general))))])
    cat(paste0(a , " = \"\",\n"))



fnWithArg <- function(arg)
    Filter(function(x)any(arg%in%x), args)



