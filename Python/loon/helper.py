import pandas as pd
### input a list/tuple/np.Series of number of strings 
### output a list of number 
def asnumeric(x):
    if(isinstance(x,pd.Series)):
        x = tuple(x)
    if(not isinstance(x[0],(int,float))):
        x = pd.Series(x,dtype='category')
        x = x.cat.codes 
    return(list(x))