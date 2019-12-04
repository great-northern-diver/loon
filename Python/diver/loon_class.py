from .tk import tk 
from .l_cget import *
from .l_configure import *
from .l_state_names import *
from sys import exit
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
        if(isinstance(value,loon_l_glyph)):
            value = value.id
        opt = {name:value}
        l_configure(self,**opt)

    # overload []   
    def __setitem__(self, name, value):
        if(isinstance(value,loon_l_glyph)):
            value = value.id
        opt = {name:value}
        l_configure(self,**opt)
    
    def __dir__(self):
        return self.names + ['plot','names']

class loon_l_plot(loon):
    '''
    l_plot
    '''
    def __init__(self, plot):
        super().__init__(plot)    


class loon_l_hist(loon):
    '''
    l_hist
    '''
    def __init__(self, plot):
        super().__init__(plot)    


class loon_l_graph(loon):
    '''
    l_graph
    '''
    def __init__(self, plot):
        super().__init__(plot)    


class loon_l_serialaxes(loon):
    '''
    l_serialaxes
    '''
    def __init__(self, plot):
        super().__init__(plot)    


class loon_l_plot3D(loon):
    '''
    l_plot3D
    '''
    def __init__(self, plot):
        super().__init__(plot)    

class loon_l_layer():
    '''
    l_layer 
    '''
    def __init__(self, widget, Type, id):
        self.__dict__['widget'] = widget
        self.__dict__['Type'] = Type 
        self.__dict__['id'] = id    
    def __getattr__(self, key):
        if(key in {'widget','Type','id'}):
            return self.key
        elif(key == 'names'):
            return l_state_names(self)
        else:
            return l_cget(self,key)
    def __getitem__(self, key):
        if(key in {'widget','Type','id'}):
            return self.key
        elif(key == 'names'):
            return l_state_names(self)
        else:
            return l_cget(self,key)
    # #overload .
    def __setattr__(self, name, value):
         opt = {name:value}
         l_configure(self,**opt)

    # overload []   
    def __setitem__(self, name, value):
        opt = {name:value}
        l_configure(self,**opt)


######## Not finished yet 

class loon_l_compound():
    '''
    l_compound class 
    '''
    def __init__(self, plot):
        self.__dict__["plot"] = plot 
        
    def __getattr__(self, key):
        if(key == 'plot'):
            return self.plot
        elif(key == 'names'):
            return list(self.plot.keys())
        else:  
            return {k: v[key] if key in v.names else None for k,v in self.plot.items()}
    def __getitem__(self, key):
        if(key == 'plot'):
            return self.plot
        elif(key == 'names'):
            return list(self.plot.keys())
        else:
            # return l_cget(self,key)
            return {k: v[key] if key in v.names else None for k,v in self.plot.items()}
    # overload .    
    def __setattr__(self, name, value):
        opt = {name:value}
        #l_configure(self,**opt)
        #[lambda x: x[name] = value for x in self.plot.values()]
        #list(map(lambda x: l_configure(x,**opt), self.plot.values()))
        [l_configure(v,**opt) if name in v.names else None for v in self.plot.values()]
    # overload []   
    def __setitem__(self, name, value):
        opt = {name:value}
        #l_configure(self,**opt)
        # [x[name] = value for x in self.plot.values()]
        #list(map(lambda x: l_configure(x,**opt), self.plot.values()))
        [l_configure(v,**opt) if name in v.names else None for v in self.plot.values()]
    
    # def __dir__(self):
    #     return list(self.plot.values())[0].names + ['plot','names']

class loon_l_pairs(loon_l_compound):
    '''
    l_pairs class 
    '''
    def __init__(self, plot):
        super().__init__(plot)
            

class loon_l_glyph():
    '''
    l_plot
    '''
    def __init__(self, widget,id,Type):
        self.__dict__['widget'] = widget
        self.__dict__['id'] = id 
        self.__dict__['type'] = Type      
    def __getattr__(self, key):
        if(key in {'widget','id','type'}):
            return self[key]
        else:  
            return l_cget(self,key)
    def __getitem__(self, key):
        if(key in {'widget','id','type'}):
            return self.plot
        else:
            return l_cget(self,key)
    # overload .
    def __setattr__(self, name, value):
        if(isinstance(value,loon_l_glyph)):
            value = value.id
        opt = {name:value}
        l_configure(self,**opt)

    # overload []   
    def __setitem__(self, name, value):
        if(isinstance(value,loon_l_glyph)):
            value = value.id
        opt = {name:value}
        l_configure(self,**opt)


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



class loon_l_savedStates:
    def __init__(self,info,obj_type):
        self.__dict__['info'] = info
        self.__dict__['type'] = obj_type
    # def __getattr__(self, key):
    #     return self.type
    def __getitem__(self, key):
        if(key in self.info.keys()):
            return self.info[key]
        else:
            exit(key + 'is not an vaild attribute in the saveState')
