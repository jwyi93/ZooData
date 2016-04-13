import pandas as pd
import numpy as np 
import re
import datetime
from itertools import tee, islice, chain, izip

############## Import Raw datafile ##############
%cd "/Users/coreyjackson/Desktop/Learning Beyond Task (CSCW 17)/Data"


raw_data = pd.read_csv('tags.csv',delimiter=',') # CHANGE NAME OF .CSV FILE

# thread created_at variable
raw_data['year'] = raw_data['created_at'].str.extract('(....-..-..)')
raw_data['time'] = raw_data['created_at'].str.extract('(..:..:..)')
raw_data['created_at'] = raw_data['year'] + " " +raw_data['time'] 
raw_data['created_at'] = pd.to_datetime(raw_data['created_at'])
del raw_data['year']
del raw_data['time']
# comment created_at variable
raw_data['year'] = raw_data['created_at_comment'].str.extract('(....-..-..)')
raw_data['time'] = raw_data['created_at_comment'].str.extract('(..:..:..)')
raw_data['created_at_comment'] = raw_data['year'] + " " +raw_data['time'] 
raw_data['created_at_comment'] = pd.to_datetime(raw_data['created_at_comment'])
del raw_data['year']
del raw_data['time']

raw_data = raw_data.sort_values(['title'], ascending=[1])

# Subset for testing purposes
test_data = raw_data[0:100]
#rename columns

# Parse data by hashtags 

# Parse data by project 
test_data['name'].value_counts()


# Projects and counts of number of hashtags
raw_data['project_id'].value_counts()

# ObjectId(5333063b3ae740228a000001)    533013
# ObjectId(502a90cd516bcb060c000001)    168973
# ObjectId(52e2cfc1806ea54590000001)    131263
# ObjectId(551d8b5c61cd354e3c000001)     82327
# ObjectId(52d065303ae740380a000001)     70498
# ObjectId(5077375154558fabd7000001)     56024
# ObjectId(50e9e3d33ae740f1f3000001)     54924
# ObjectId(516d6f243ae740bc96000001)     31140
# ObjectId(52afdb804d69636532000001)     29239
# ObjectId(5101a1341a320ea77f000001)     15872
# ObjectId(545b9c49b0c7823037000001)     13401
# ObjectId(528ceac53ae7409ca0000001)     12133
# ObjectId(54115cd012fc4d17e2000001)      9661
# ObjectId(54f42c0ab35d2e06bd000001)      8998
# ObjectId(511410da3ae740c3ec000001)      8026
# ObjectId(523ca1a03ae74053b9000001)      6832
# ObjectId(536d226d3ae740fd20000001)      5270
# ObjectId(5410f2c03ae740bdc0000001)      4141
# ObjectId(52e2cba83ae7401db5000001)      3415
# ObjectId(51e6fcdd3ae74023b9000001)      2722
# ObjectId(51c9bba83ae7407725000001)      2377
# ObjectId(503293e6516bcb6782000001)      1291
# ObjectId(52d1718e3ae7401cc8000001)       834
# ObjectId(51c1c9523ae74071c0000001)       351
# ObjectId(5527cafa38d2313a2a000001)        59
# ObjectId(53c94845edf877f277000001)         3

# Export data 
raw_data.to_csv('Proejct_Tags.csv') #Change File name to project name. 
