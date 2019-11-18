import pandas as pd
def l_data(data):
    """
    Convert an R data.frame to a Tcl dictionary
    
    Description:
        This is a helper function to convert an R data.frame object to 
        a Tcl data frame object. This function is useful when changing a data state
        with l_configure 
    Args:
        data: a pandas dataframe object
    Returns:
        a string that represents with data.frame with a Tcl dictionary data
        structure.
    @namespace loon.l_data
    """
    if (type(data) != pd.DataFrame):
        exit("data is expected to be a pandas DataFrame")
    ## Create a dict
    var_names = data.columns
    val = [data.iloc[:,i] for i in range(data.shape[1])]
    res = ''
    for i in range(len(val)):
        val[i] =  [str(val[i][x]) for  x in range(len(val[i]))]
        res += ' {' + str(var_names[i]) + '} '
        res += '{ ' + ' '.join(val[i]) + '}' 
    return(res)
