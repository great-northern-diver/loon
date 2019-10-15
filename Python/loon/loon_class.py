
from tk import tk 
class loon:
    """
    Loon class 
    """
    def __init__(self, plot,type):
        self.plot = plot
        self.type = type
        #self.color = tk.eval()
    def __getattr__(self, name):
        if(name == 'plot'):
            return self.plot
        elif(name == 'names'):
            try:    
                tk.eval(self.plot + ' cget -option')
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
                tk.eval(self.plot + ' cget -option')
            except Exception as e:
                temp = str(e)
            res = temp.split('-')[1:]
            res = [x.split(',')[0] for x in res]
            return res
        else:
            return tk.eval(self.plot + ' cget -' + key)  

    def __setattr__(self, name, value):
        self.__dict__[name] = value
    #def __setitem__(self, name, value):
