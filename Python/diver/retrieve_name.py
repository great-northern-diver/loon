import inspect 
def retrieve_name(var):
    temp = inspect.currentframe().f_back.f_back.f_globals.items()
    res = [var_name for var_name, var_val in temp if var_val is var]
    return(res)
    # if(len(res) > 0):
    #     return res
    # else:
        
    #     temp = inspect.currentframe().f_back.f_back.f_back.f_globals.items()
    #     print(temp)
    #     res = [var_name for var_name, var_val in temp if var_val is var]
    #     return(res)
        