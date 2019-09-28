"""@package docstring
Documentation for this module. 

More details.
"""
import tkinter
import inspect 
from tkinter import *
from collections import defaultdict

tk = tkinter.Tk()
tk.withdraw() 
tk.eval('lappend auto_path /Users/tedwang/Desktop/loon/Tcl')
tk.eval('package require loon')
#p = tk.eval('set p [loon::plot -x {1 2 3} -y {1 2 3}]')

def retrieve_name(var):
    temp = inspect.currentframe().f_back.f_globals.items()
    return [var_name for var_name, var_val in temp if var_val is var]

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

####################
def loonPlotFactory(factory_tclcmd,factory_path,factory_window_title="loon plot", parent=None, options=None):
    """Documentation for a function.
    More details.
    """
    new_toplevel = False
    if(parent == None):
        new_toplevel = True
        parent = l_toplevel()
    child = l_subwin(parent, factory_path)
    if(options == None):
        plot = tk.tk.call(factory_tclcmd, child)
    else:
        opt = []
        for key, value in options.items():
            opt.append('-' + key)
            opt.append(value)
        plot = tk.tk.call(factory_tclcmd,child,*opt)
    if(new_toplevel):
        tk.tk.call('pack', plot,'-expand',1,'-fill','both')
        tk.tk.call('wm','title',parent, factory_window_title +" "+ str(plot))
    plot = str(plot)
    return(plot)

def l_plot(x=None, y=None, parent=None, options = None):
    """Documentation for a function.
    More details.
    """
    if(x == None):      
        plot = loonPlotFactory('::loon::plot', 'plot', 'loon scatterplot', parent)
    else:        
        ## Get x, y, xlab and ylab
        ## similar as in plot.default use xy.coords
        xlabel = retrieve_name(x) 
        ylabel = retrieve_name(y) 
        plot = loonPlotFactory('::loon::plot', 'plot', 'loon scatterplot', parent,
                                options= {'x':x,'y':y,'xlabel':xlabel,'ylabel':ylabel})
        plot = loon_obj(plot)
    return(plot)

def l_hist(x=None, origin=None, binwidth=None, parent=None, options = None):
    """Documentation for a function.
    More details.
    """
    if(x == None):
        plot = loonPlotFactory('::loon::histogram', 'hist', 'loon histogram', parent, options)
    else:
        xlabel = retrieve_name(x)        
        if(origin == None):
            origin = min(x)
        if(binwidth == None):
            binwidth = (max(x) - min(x))/30
            if(binwidth < 0.0001):
                binwidth = 0.00005
        plot = loonPlotFactory('::loon::histogram', 'hist', 'loon histogram', parent,
                                options= {'x':x, 'origin':origin, 'binwidth':binwidth,'xlabel':xlabel})
        plot = loon_obj(plot)
    return(plot)

def l_data(data):
    #if (!is.data.frame(data)) stop("data is expected to be a data.frame")
    if (type(data) != pd.DataFrame):
        exit("data is expected to be a pandas DataFrame")
    ## Create a dict
    var_names = data.columns
    val = [data.iloc[:,i] for i in range(len(data))]
    res = ''
    for i in range(len(val)):
        val[i] =  [str(val[i][x]) for  x in range(len(val[i]))]
        res += ' {' + str(var_names[i]) + '} '
        res += '{ ' + ' '.join(val[i]) + '}' 
    return(res)

def l_serialaxes(data, sequence=None, scaling="variable", axesLayout='radial',showAxes=TRUE, parent=None,options =  None):
    data = pd.DataFrame(data)                            
    if(sequence == None):
        sequence = list(data.columns)
    plot = loonPlotFactory('::loon::serialaxes', 'serialaxes', 'loon serialaxes plot', parent,
                    {"data":l_data(data),
                    "sequence":sequence,
                    "showAxes":showAxes,
                    "scaling":scaling,
                    "axesLayout":axesLayout})
    plot = loon_obj(plot)
    return(plot)

class loon_obj:
    """Documentation for a class.
 
    More details.
    """
    def __init__(self, plot):
        self.plot = plot
        #self.color = tk.eval()
    def __getattr__(self, name):
        if(name == 'plot'):
            return self.plot
        elif(name == 'names'):
            try:    
                tk.eval(p.plot + ' cget -option')
            except Exception as e:
                temp = str(e)
            res = temp.split('-')[1:]
            res = [x.split(',')[0] for x in res]
            return res
        else:
            return tk.eval(self.plot + ' cget -' + name)      
    def __getitem__(self, key):
        if(key == 'plot'):
            return self.plot
        elif(key == 'names'):
            try:    
                tk.eval(p.plot + ' cget -option')
            except Exception as e:
                temp = str(e)
            res = temp.split('-')[1:]
            res = [x.split(',')[0] for x in res]
            return res
        else:
            return tk.eval(self.plot + ' cget -' + key)  


height = [1,2,1]
width = [1,2,3]    
p = l_plot(height,width)
p = l_plot([1,2,3],[1,2,3])
p2 = l_plot([1,2,3],[1,2,3])
p = l_hist(height)
import pandas as pd
import numpy as np
ds = pd.DataFrame(np.arange(9).reshape(3,3))
l_serialaxes(ds)

