def loonPlotFactory(factory_tclcmd,factory_path,factory_window_title="loon plot", parent=None, options=None):
    """Documentation for a function.
    More details.
    """
    new_toplevel = False
    if(parent == None):
        new_toplevel = True
        parent = l_toplevel()
    child = l_subwin(parent, factory_path)
    #plot = try(tcl(factory_tclcmd, child, ...))
    #print("child: ", child)
    #print("parent: ", parent)
    if(options == None):
        plot = tk.tk.call(factory_tclcmd, child)
    else:
        opt = []
        for key, value in options.items():
            opt.append('-' + key)
            opt.append(value)
        plot = tk.tk.call(factory_tclcmd,child,*opt)
        #plot = tk.tk.call(factory_tclcmd,child,'-x',options['x'],'-y',options['y'],'-xlabel',options['xlabel'],'-ylabel',options['ylabel'])
    #############
    if(new_toplevel):
        #tkpack(plot,  fill="both", expand=TRUE)
        tk.tk.call('pack', plot,'-expand',1,'-fill','both')
        #tktitle(parent) <- paste(factory_window_title, plot)
        tk.tk.call('wm','title',parent, factory_window_title +" "+ plot)
        #tk.tk.call('bind',parent,"<FocusIn>",'+::loon::setLoonInspectorActiveWidget',plot)
        #tk.eval('bind ' + parent + " <FocusIn> +::loon::setLoonInspectorActiveWidget "+ plot)
        ## Bind Ctrl-P to export image
        #tcl("bind", parent, "<Control-KeyPress-p>",
        #    function()exportImageDialog(plot))
        #tcl("bind", parent, "<Control-KeyPress-P>",
        #    function()exportImageDialog(plot))
    ###############
    plot = str(plot)
    #class(plot) <- "loon"
    return(plot)
