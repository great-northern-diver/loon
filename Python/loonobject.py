
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

