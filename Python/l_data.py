def l_data(data):
    #if (!is.data.frame(data)) stop("data is expected to be a data.frame")
    if (type(data) != pd.DataFrame):
        exit("data is expected to be a pandas DataFrame")
    ## Create a dict
    var_names = data.columns
    val = [data.iloc[:,i] for i in range(len(data))]
    res = ''
    for i in range(len(val)):
        val[i] =  [str(val[i][x]) for  x in range(len(val[i]))]
        res += ' {' + var_names[i] + '} '
        res += '{ ' + ' '.join(val[i]) + '}' 
    return(res)



