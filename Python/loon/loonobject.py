
from l_throwErrorIfNotLoonWidget import *
from tk import tk
import loon_class
def loonobject(target, convert=str):
    """    
    Create closure to evaluate code for a loon object
       
        Returns a closure that evaluates code for a partiular part of a 
        loon plot widget such as a: the widget itself, layer, glyph,
        navigator, or context.
    
    Args:
        target: target loon object
    Returns:
        a closure that will evaluate tcl code for the particular object.
    """
    ## first check for loon objects
    # if (target.type == 'l_layer' or target.type == 'l_glyph'):
    #     specifier = 0
    #     Type = 'widget'
    # elif (target.type == 'l_navigator'):        
    #     specifier = 0
    #     Type = 'widget'
    # elif (target.type == 'l_context'):
    #     specifier = 0     
    #     Type = 'widget'
    if (isinstance(target,loon_class.loon_l_layer) or isinstance(target,loon_class.loon_l_glyph) ):
        loon_obj = target
        specifier = [target.widget,target.id]
        Type = type(target).__name__[7:]
        hasRecognized = True
    elif (isinstance(target,loon_class.loon_l_navigator)):        
        specifier = 0
        Type = 'widget'
    elif (isinstance(target,loon_class.loon_l_context)):
        specifier = 0     
        Type = 'widget'
    else: 
        ## strip attributes
        specifier = [target.plot]

        # type <- switch(length(specifier),
        #                '1'= "widget",
        #                '2'= {
        #                    switch(substr(specifier[2], 1, 5),
        #                           layer="layer",
        #                           glyph="glyph",
        #                           navig="navigator",
        #                           stop(paste0("Invalid target specifier: ",
        #                                       target))
        #                    )
        #                },
        #                '3' = {
        #                    if(substr(specifier[2], 1, 5) != "navig" ||
        #                       substr(specifier[3], 1, 5) != "conte") {
        #                        stop(paste0("Invalid target specifier: ", target))
        #                    }
        #                    "context"
        #                },
        #                stop(paste0("Invalid target specifier: ", target))
        # )
        Type = 'widget'
        
        # loon_obj <- switch(type,
        #                    widget=structure(specifier, class="loon"),
        #                    context=structure(specifier[3],
        #                                      widget=specifier[1],
        
        #                                      navigator=specifier[2],
        #                                      class="loon"),
        #                    structure(specifier[2], widget=specifier[1],
        #                              class=c("loon", paste0("l_", type))))
    widget = specifier[0]
    #widget <- specifier[1]
    l_throwErrorIfNotLoonWidget(widget)
    # call <- switch(type,
    #                widget=widget,
    #                layer=c(widget, 'layer', 'use', specifier[2]),
    #                glyph=c(widget, 'glyph', 'use', specifier[2]),
    #                navigator=c(widget, 'navigator', 'use', specifier[2]),
    #                context=c(widget, 'navigator', 'use', specifier[2],
    #                          'context', 'use', specifier[3]))
    #call = Type  
    if(Type == 'widget'):
        call = [widget]
    elif(Type == 'layer'):
        call = [widget,'layer','use',specifier[1]] 
    else:
        print('not finished yet ~~~~~')

    def fun(*args):
        args = call + list(args)
        return convert(str(tk.tk.call(*args)))
    return  fun