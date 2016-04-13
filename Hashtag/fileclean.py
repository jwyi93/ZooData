import pandas as pd
import numpy as np 
import re
import datetime
from itertools import tee, islice, chain, izip

############## Import Raw datafile ##############
%cd ""
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

# Export data 
raw_data.to_csv('Proejct_Tags.csv') #Change File name to project name. 
