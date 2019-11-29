
# Create closure to evaluate code for a loon object
#
# Returns a closure that evaluates code for a partiular part of a
# loon plot widget such as a: the widget itself, layer, glyph,
# navigator, or context.
#
# @template param_target
#
# @return a closure that will evaluate tcl code for the particular
# object.
#
# # @export .loonobject
.loonobject <- function(target, convert=as.character) {

    ## first check for loon objects
    if (is(target,'l_layer') || is(target,'l_glyph')){
        loon_obj <- target
        specifier <- c(attr(target, 'widget'), as.vector(target))
        type <-  substring(class(target)[2], 3)
        hasRecognized <- TRUE

    } else if (is(target, 'l_navigator')) {
        loon_obj <- target
        specifier <- c(attr(target, 'widget'), as.vector(target))
        type <-  substring(class(target)[1], 3)
        hasRecognized <- TRUE

    } else if (is(target, 'l_context')) {
        loon_obj <- target
        specifier <- c(attr(target, 'widget'),
                       attr(target, 'navigator'),
                       as.vector(target))
        type <- "context"
        hasRecognized <- TRUE

    } else {
        ## strip attributes
        specifier <- vapply(target, as.vector, character(1), USE.NAMES=FALSE)

        type <- switch(length(specifier),
                       '1'= "widget",
                       '2'= {
                           switch(substr(specifier[2], 1, 5),
                                  layer="layer",
                                  glyph="glyph",
                                  navig="navigator",
                                  stop(paste0("Invalid target specifier: ",
                                              target))
                           )
                       },
                       '3' = {
                           if(substr(specifier[2], 1, 5) != "navig" ||
                              substr(specifier[3], 1, 5) != "conte") {
                               stop(paste0("Invalid target specifier: ", target))
                           }
                           "context"
                       },
                       stop(paste0("Invalid target specifier: ", target))
        )

        loon_obj <- switch(type,
                           widget=structure(specifier, class="loon"),
                           context=structure(specifier[3],
                                             widget=specifier[1],
                                             navigator=specifier[2],
                                             class="loon"),
                           structure(specifier[2], widget=specifier[1],
                                     class=c("loon", paste0("l_", type)))
        )
    }

    widget <- specifier[1]
    l_throwErrorIfNotLoonWidget(widget)

    call <- switch(type,
                   widget=widget,
                   layer=c(widget, 'layer', 'use', specifier[2]),
                   glyph=c(widget, 'glyph', 'use', specifier[2]),
                   navigator=c(widget, 'navigator', 'use', specifier[2]),
                   context=c(widget, 'navigator', 'use', specifier[2],
                             'context', 'use', specifier[3]))

    function(...) {
        convert(do.call('tcl', append(call, list(...))))
    }
}
