import pandas as pd
import numpy as np 
import re
import datetime
from itertools import tee, islice, chain, izip
# This is a test

############## Import Raw datafile ##############
%cd "/"

gs = pd.read_csv('.csv') # CHANGE NAME OF .CSV FILE
# Rename columns 
 # Need to first parse date/time to new field 
gs['year'] = gs['start'].str.extract('(....-..-..)')
gs['time'] = gs['start'].str.extract('(..:..:..)')
gs['datetime'] = gs['year'] + " " +gs['time'] 
gs['datetime'] = pd.to_datetime(gs['datetime'])

# Delete the fields we created and those that are unnecessary
del gs['year']
del gs['time']
#del gs['tutorial']
#del gs['favorite']

# Sort dataframe by user by date
gs = gs.sort_values(['userID','datetime'], ascending=[1, 1])

# Makes new column for user ip 
gs['same_userID'] = gs['userID'].shift() == gs['userID']

# Shifts up one removing first observation, last is NA (for ip)
gs.same_userID = gs.same_userID.shift(-1)

# Makes new column for datetime 
gs['datetime2'] = gs['datetime'] 

# Shifts up one removing first observation, last is NA (for datetime)
gs.datetime2 = gs.datetime2.shift(-1)

# Changes variabel to datetime variable
gs['datetime'] = pd.to_datetime(gs['datetime'])
gs['datetime2'] = pd.to_datetime(gs['datetime2'])

# Subtract time
gs['timespent'] = gs['datetime2'] - gs['datetime']

# Function for iterating 
def previous_and_next(some_iterable):
    prevs, items, nexts = tee(some_iterable, 3)
    # prevs = chain([None], prevs)
    prevs = chain([0], prevs)
    next# s = chain(islice(nexts, 1, None), [None])
    nexts = chain(islice(nexts, 1, None), [0])
    return izip(prevs, items, nexts)

# Count through the number of annotation by ip address
ip = gs['userID']
classification_no = []
for previous, item, nxt in previous_and_next(ip):
  if item == previous:  	
	classification = classification + 1
	classification_no.append(classification)
   # print "Item is now", item, "next is", nxt, "previous is", previous
  else:
	classification = 1
	classification_no.append(classification)
gs['Classifications'] = classification_no


# Loop to iterate and create session variable by ip address
time = gs['timespent']
ip = gs['userID']
session_no = []
session = 1
for i,j,l,m in zip(ip, ip[1:], time, time[1:]):
  #print i,j,l,m
  if i == j and l <= datetime.timedelta(minutes=30):
    session = session
    session_no.append(session)
  elif i == j and l > datetime.timedelta(minutes=30): 
    session = session + 1
    session_no.append(session)
  else :  
    session = 1
    session_no.append(session)

# Check length of anon file and session list 
len(gs)
len(session_no)           
# Add one element to beginning of list. Required for appending list
session_no.insert(0,1)
del gs['datetime2']
# Paste list to gs dataframe 
gs['Session'] = session_no
#gs.Session = gs.Session.shift(-1)

time = gs['timespent']
time_sec = []
for i in time:
  timeseconds = i.total_seconds()
  time_sec.append(timeseconds)
gs['Time_Seconds'] = time_sec


# Export dataframe
gs.to_csv('class_GS.csv') #Change File name to project name. 

 