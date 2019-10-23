#### about l_layer 
from sklearn.neighbors import KernelDensity

import pandas as pd
import numpy as np
import pylab as pl
import scipy.stats
from dataset import iris
dd =iris.iloc[:,1]
pdf = scipy.stats.kde.gaussian_kde(dd)
x = np.linspace((dd.min()-1),(dd.max()+1), len(dd)) 
y = pdf(x)     


#### about color
import seaborn
seaborn.color_palette()