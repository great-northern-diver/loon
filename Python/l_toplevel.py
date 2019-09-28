def l_toplevel(path= None):
    """Documentation for a function.
    More details.
    """
    if(path == None):
        i = 0
        child = '.l'+ str(i)
        while(int(tk.eval('winfo exists ' + child))):   
            i = i + 1
            child = '.l'+ str(i)                    
        path = child
        tt = str(tk.eval('toplevel ' + path))
    #print("tt: ",tt)
    tk.eval('wm iconphoto ' + tt + ' -default ::loon::loonIcon')    
    return(tt)
