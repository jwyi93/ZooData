# Corey Jackson
# Syracuse University, 2016

import pandas as pd
import numpy as np 
import re
import datetime
from itertools import tee, islice, chain, izip
# This is a test

############## Import Raw datafile ##############
#cd "/Users/coreyjackson/Desktop/Research Projects/Anonymous Work CHI 2017/Archive"

raw_data = pd.read_csv('gravity-spy-classifications.csv') # CHANGE NAME OF .CSV FILE
# Rename columns 
#anon_population = pd.DataFrame(raw_data, columns = ['_id', 'created_at', 'favorite', 'subject_ids', 'tutorial','user_id','user_ip','user_name'])
#del raw_data
# Need to first parse date/time to new field 
raw_data['year'] = raw_data['created_at'].str.extract('(....-..-..)')
raw_data['time'] = raw_data['created_at'].str.extract('(..:..:..)')
raw_data['datetime'] = raw_data['year'] + " " +raw_data['time'] 
raw_data['datetime'] = pd.to_datetime(raw_data['datetime'])

# Delete the fields we created and those that are unnecessary
del raw_data['year']
del raw_data['time']
del raw_data['created_at']
#del anon_population['tutorial']
#del anon_population['favorite']

# Remove blank user names 
raw_data = raw_data.sort_values(['user_name','datetime'], ascending=[1, 1])

# Makes new column for user ip 
raw_data['same_name'] = raw_data['user_name'].shift() == raw_data['user_name']

# Shifts up one removing first observation, last is NA (for ip)
#anon_known.same_ip = anon_known.same_ip.shift(-1)

# Makes new column for datetime 
raw_data['datetime2'] = raw_data['datetime'] 

# Shifts up one removing first observation, last is NA (for datetime)
raw_data.datetime2 = raw_data.datetime2.shift(1)

# Changes variabel to datetime variable
raw_data['datetime'] = pd.to_datetime(raw_data['datetime'])
raw_data['datetime2'] = pd.to_datetime(raw_data['datetime2'])

# Subtract time
raw_data['timespent'] = raw_data['datetime2'] - raw_data['datetime']

time = raw_data['timespent']
time_sec = []
for i in time:
  timeseconds = i.total_seconds()
  time_sec.append(timeseconds)
raw_data['Time_Seconds'] = time_sec
raw_data['Time_Seconds'] = raw_data['Time_Seconds']*(-1)

# Function for iterating 
def previous_and_next(some_iterable):
    prevs, items, nexts = tee(some_iterable, 3)
    # prevs = chain([None], prevs)
    prevs = chain([0], prevs)
    next# s = chain(islice(nexts, 1, None), [None])
    nexts = chain(islice(nexts, 1, None), [0])
    return izip(prevs, items, nexts)

# Count through the number of annotation by ip address
ip = raw_data['user_name']
classification_no = []
for previous, item, nxt in previous_and_next(ip):
  if item == previous:  	
	classification = classification + 1
	classification_no.append(classification)
   # print "Item is now", item, "next is", nxt, "previous is", previous
  else:
	classification = 1
	classification_no.append(classification)
raw_data['Classifications'] = classification_no

# Loop to iterate and create session variable by ip address
time = raw_data['Time_Seconds']
ip = raw_data['user_ip']
same = raw_data['same_name']
session_no = []
session = 1
for i,j,l,m,n in zip(ip, ip[1:], time, time[1:],same):
  #print i,j,l,m,n
  if n == True and l <= 1800:
    session = session
    session_no.append(session)
  elif n == True and l > 1800: 
    session = session + 1
    session_no.append(session)
  else :  
    session = 1
    session_no.append(session)

# Check length of anon file and session list 
len(raw_data)
len(session_no)           
# Add one element to beginning of list. Required for appending list
session_no.extend([1])
del raw_data['datetime2']
# Paste list to anon_known dataframe 
raw_data['Session'] = session_no
#anon_known.Session = anon_known.Session.shift(-1)

# Export dataframe
raw_data.to_csv('gravity-spy-classifications-sessions-classifications.csv') #Change File name to project name. 

