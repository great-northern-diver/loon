import inspect 
def retrieve_name(var):
    temp = inspect.currentframe().f_back.f_back.f_globals.items()
    return [var_name for var_name, var_val in temp if var_val is var]