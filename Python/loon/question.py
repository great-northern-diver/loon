#### about l_layer 
from sklearn.neighbors import KernelDensity

import pandas as pd
import numpy as np
import pylab as pl
import scipy.stats
pdf = scipy.stats.kde.gaussian_kde(df.iloc[:,1])
x = np.linspace((df.min()-1),(df.max()+1), len(df)) 
y = pdf(x)     