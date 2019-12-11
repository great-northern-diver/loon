
from .l_throwErrorIfNotLoonWidget import *
from .tk import tk
from . import loon_class
import numpy as np
def loonobject(target, convert=str):
    """    
    Create closure to evaluate code for a loon object

    Description:   
        Returns a closure that evaluates code for a partiular part of a 
        loon plot widget such as a: the widget itself, layer, glyph,
        navigator, or context.
    
    Args:
        target: target loon object
    Returns:
        a closure that will evaluate tcl code for the particular object.
    @namespace loon.loonobject
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
        loon_obj = target
        specifier = [target.widget,target.id] 
        Type = type(target).__name__[7:]
        hasRecognized = True
    elif (isinstance(target,loon_class.loon_l_context)):
        loon_obj = target
        specifier = [target.widget,target.navigator,target.id] 
        Type = "context"
        hasRecognized = True
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
    elif(Type == 'glyph'):
        call = [widget, 'glyph', 'use', specifier[1]]
    elif(Type == 'navigator'):
        call = [widget, 'navigator', 'use', specifier[1]]
    elif(Type == 'context'):
        call = [widget, 'navigator', 'use', specifier[1],
                    'context', 'use', specifier[2]]
    else:
        print('not finished yet ~~~~~')
    def fun(*args):
        #plot = tk.tk.call(factory_tclcmd,child,*opt)
        #convert(do.call('tcl', append(call, list(...))))
        args = call + list(args)
        temp = tk.tk.call(*args)
        if(isinstance(temp,tuple)):
            return np.array(temp).flatten()
        elif(isinstance(temp,(int,float))):
            return temp
        else:
            return convert(str(temp))
    return  fun