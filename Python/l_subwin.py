def l_subwin(parent, name="w"):
    """Documentation for a function.
    More details.
    """
    if(parent == "tkwin"): 
        parent = parent.winfo_id()    
    i = 0
    child = parent + '.'+ name
    while(int(tk.eval('winfo ' + 'exists '+ child))):       
        i = i + 1
        child = parent+ '.'+ name+ i        
    return(child)
