from tk import tk 
from l_cget import *
from l_configure import *
from l_state_names import *

class loon:
    """
    Loon class 
    """
    def __init__(self, plot):
        self.__dict__['plot'] = plot
    def __getattr__(self, key):
        if(key == 'plot'):
            return self.plot
        elif(key == 'names'):
            return l_state_names(self)
        else:  
            return l_cget(self,key)
    def __getitem__(self, key):
        if(key == 'plot'):
            return self.plot
        elif(key == 'names'):
            return l_state_names(self)
        else:
            return l_cget(self,key)
    # overload .
    def __setattr__(self, name, value):
         opt = {name:value}
         l_configure(self,**opt)

    # overload []   
    def __setitem__(self, name, value):
        opt = {name:value}
        l_configure(self,**opt)


class loon_l_plot(loon):
    '''
    l_plot
    '''
    def __init__(self, plot):
        super().__init__(plot)    


class loon_l_hist(loon):
    '''
    l_plot
    '''
    def __init__(self, plot):
        super().__init__(plot)    


class loon_l_graph(loon):
    '''
    l_plot
    '''
    def __init__(self, plot):
        super().__init__(plot)    


class loon_l_serialaxes(loon):
    '''
    l_plot
    '''
    def __init__(self, plot):
        super().__init__(plot)    


class loon_l_plot3D(loon):
    '''
    l_plot
    '''
    def __init__(self, plot):
        super().__init__(plot)    


######## Not finished yet 
class loon_l_layer(loon):
    '''
    l_plot
    '''
    def __init__(self, plot):
        super().__init__(plot)    
    def __getattr__(self, key):
        if(key == 'plot'):
            return self.plot
        elif(key == 'names'):
            try:    
                tk.eval(self.plot + ' cget -option')
            except Exception as e:
                temp = str(e)
            res = temp.split('-')[1:]
            res = [x.split(',')[0] for x in res]
            return res
        else:
            return tk.eval(self.plot + ' cget -' + key)      
            #return l_cget(self,key)
    def __getitem__(self, key):
        if(key == 'plot'):
            return self.plot
        elif(key == 'names'):
            try:    
                tk.eval(self.plot + ' cget -option')
            except Exception as e:
                temp = str(e)
            res = temp.split('-')[1:]
            res = [x.split(',')[0] for x in res]
            return res
        else:
            return tk.eval(self.plot + ' cget -' + key)  
            #return l_cget(self,key)
    # #overload .
    def __setattr__(self, name, value):
    #     #self.__dict__[name] = value
         opt = {name:value}
         l_configure(self,**opt)
    #     #l_configure(self,name,value)

    # overload []   
    def __setitem__(self, name, value):
        opt = {name:value}
        l_configure(self,**opt)


class loon_l_glyph(loon):
    '''
    l_plot
    '''
    def __init__(self, plot):
        super().__init__(plot)    


class loon_l_navigator(loon):
    '''
    l_plot
    '''
    def __init__(self, plot):
        super().__init__(plot)    

class loon_l_context(loon):
    '''
    l_plot
    '''
    def __init__(self, plot):
        super().__init__(plot)    
