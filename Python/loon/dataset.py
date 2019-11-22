import pandas as pd
from pathlib import Path
#working_dir = './loon'
working_dir = str(Path(__file__).resolve().parent)
 
## iris data set 
#  @namespace loon.iris
iris = pd.read_csv(working_dir+'/datasets/iris.csv')


## olive 
#  @namespace loon.olive

olive = pd.read_csv(working_dir + '/datasets/olive.csv')


## oliveAcids 
#  @namespace loon.oliveAcids

oliveAcids = pd.read_csv(working_dir + '/datasets/oliveAcids.csv')


## oliveLocations data set 
#  @namespace loon.oliveLocations

oliveLocations = pd.read_csv(working_dir + '/datasets/oliveLocations.csv')





## quakes data set 
# @namespace loon.quakes

quakes = pd.read_csv(working_dir + '/datasets/quakes.csv')



## faithful data set 
#  @namespace loon.faithful

faithful = pd.read_csv(working_dir + '/datasets/faithful.csv')