import pandas as pd
import numpy as np
### input a list/tuple/np.Series of number of strings 
### output a list of number 
def asnumeric(x):
    if(isinstance(x,pd.Series)):
        x = tuple(x)
    if(not isinstance(x[0],(int,float))):
        x = pd.Series(x,dtype='category')
        x = x.cat.codes 
    return(list(x))

def match_arg(arg,options, argname):
    if(not isinstance(arg,str)):
        exit(argname + ' must be a sting')
    res =  [x.find(arg) for x in options]
    if(res == [0,0] or res == [-1,-1]):
        exit( argname + ' should be one of ["' + '","'.join(options) +  '"]')
    else:
        return(options[res.index(0)])

def opts_to_list(opt):
    res = []
    for key, value in opt.items():
        res.append('-' + key)
        res.append(value)
    return res

