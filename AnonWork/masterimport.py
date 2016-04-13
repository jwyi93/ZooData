import pandas as pd
import numpy as np 
import re
import datetime
from itertools import tee, islice, chain, izip
# This is a test

############## Import Raw datafile ##############
%cd "PATH TO FOLDER"

raw_data = pd.read_csv('FILE NAME') # CHANGE NAME OF .CSV FILE
# Rename columns 
anon_population = pd.DataFrame(raw_data, columns = ['_id', 'created_at', 'favorite', 'subject_ids', 'tutorial','user_id','user_ip','user_name'])
del raw_data
# Need to first parse date/time to new field 
anon_population['year'] = anon_population['created_at'].str.extract('(....-..-..)')
anon_population['time'] = anon_population['created_at'].str.extract('(..:..:..)')
anon_population['datetime'] = anon_population['year'] + " " +anon_population['time'] 
anon_population['datetime'] = pd.to_datetime(anon_population['datetime'])

# Delete the fields we created and those that are unnecessary
del anon_population['year']
del anon_population['time']
del anon_population['created_at']
#del anon_population['tutorial']
#del anon_population['favorite']

# Sort dataframe by user by date
anon_population = anon_population.sort_values(['user_ip','datetime'], ascending=[1, 1])

# Makes new column for user ip 
anon_population['same_ip'] = anon_population['user_ip'].shift() == anon_population['user_ip']

# Shifts up one removing first observation, last is NA (for ip)
anon_population.same_ip = anon_population.same_ip.shift(-1)

# Makes new column for datetime 
anon_population['datetime2'] = anon_population['datetime'] 

# Shifts up one removing first observation, last is NA (for datetime)
anon_population.datetime2 = anon_population.datetime2.shift(-1)

# Changes variabel to datetime variable
anon_population['datetime'] = pd.to_datetime(anon_population['datetime'])
anon_population['datetime2'] = pd.to_datetime(anon_population['datetime2'])

# Subtract time
anon_population['timespent'] = anon_population['datetime2'] - anon_population['datetime']

# Function for iterating 
def previous_and_next(some_iterable):
    prevs, items, nexts = tee(some_iterable, 3)
    # prevs = chain([None], prevs)
    prevs = chain([0], prevs)
    next# s = chain(islice(nexts, 1, None), [None])
    nexts = chain(islice(nexts, 1, None), [0])
    return izip(prevs, items, nexts)

# Count through the number of annotation by ip address
ip = anon_population['user_ip']
classification_no = []
for previous, item, nxt in previous_and_next(ip):
  if item == previous:  	
	classification = classification + 1
	classification_no.append(classification)
   # print "Item is now", item, "next is", nxt, "previous is", previous
  else:
	classification = 1
	classification_no.append(classification)
anon_population['Classifications'] = classification_no


# Loop to iterate and create session variable by ip address
time = anon_population['timespent']
ip = anon_population['user_ip']
session_no = []
session = 1
for i,j,l,m in zip(ip, ip[1:], time, time[1:]):
  #print i,j,l,m
  if i == j and l < datetime.timedelta(minutes=30):
    session = session
    session_no.append(session)
  elif i == j and l > datetime.timedelta(minutes=30): 
    session = session + 1
    session_no.append(session)
  else :  
    session = 1
    session_no.append(session)

# Check length of anon file and session list 
len(anon_population)
len(session_no)           
# Add one element to beginning of list. Required for appending list
session_no.insert(0,1)
del anon_population['datetime2']
# Paste list to anon_population dataframe 
anon_population['Session'] = session_no
#anon_population.Session = anon_population.Session.shift(-1)

time = anon_population['timespent']
time_sec = []
for i in time:
  timeseconds = i.total_seconds()
  time_sec.append(timeseconds)
anon_population['Time_Seconds'] = time_sec


# Export dataframe
%cd "PATH TO FOLDER"

anon_population.to_csv('FILE NAME') #Change File name to project name. 


