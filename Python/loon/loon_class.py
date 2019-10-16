
from tk import tk 
from l_cget import *
from l_configure import *
class loon:
    """
    Loon class 
    """
    def __init__(self, plot,type):
        self.plot = plot
        self.type = type
        #self.color = tk.eval()
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
    # def __setattr__(self, name, value):
    #     #self.__dict__[name] = value
    #     opt = {name:value}
    #     l_configure(self,**opt)
    #     #l_configure(self,name,value)

    # overload []   
    def __setitem__(self, name, value):
        opt = {name:value}
        l_configure(self,**opt)