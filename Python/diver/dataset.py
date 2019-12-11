import pandas as pd
from pathlib import Path
#working_dir = './loon'
working_dir = str(Path(__file__).resolve().parent)
 
## iris data set 
iris = pd.read_csv(working_dir+'/datasets/iris.csv')


## olive 

olive = pd.read_csv(working_dir + '/datasets/olive.csv')


## oliveAcids 

oliveAcids = pd.read_csv(working_dir + '/datasets/oliveAcids.csv')


## oliveLocations data set 

oliveLocations = pd.read_csv(working_dir + '/datasets/oliveLocations.csv')





## quakes data set 

quakes = pd.read_csv(working_dir + '/datasets/quakes.csv')



## faithful data set 

faithful = pd.read_csv(working_dir + '/datasets/faithful.csv')


## UsAndThem data set
UsAndThem = pd.read_csv(working_dir + '/datasets/UsAndThem.csv')