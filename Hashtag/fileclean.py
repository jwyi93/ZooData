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

## TF-IDF functions
# http://aimotion.blogspot.com/2011/12/machine-learning-with-python-meeting-tf.html
import math
from textblob import TextBlob as tb

def tf(word, blob):
    return blob.words.count(word) / len(blob.words)

def n_containing(word, bloblist):
    return sum(1 for blob in bloblist if word in blob)

def idf(word, bloblist):
    return math.log(len(bloblist) / (1 + n_containing(word, bloblist)))

def tfidf(word, blob, bloblist):
    return tf(word, blob) * idf(word, bloblist)

##### Code to produce common terms

# Put all words for a project in a single document (e.g., document3 = tb("word1 word2"))  


# Create documents at T1 (3 month), T2 (6 month), T3 (9 month), T4 (12 month) 



bloblist = [document1, document2, document3]
for i, blob in enumerate(bloblist):
    print("Top words in document {}".format(i + 1))
    scores = {word: tfidf(word, blob, bloblist) for word in blob.words}
    sorted_words = sorted(scores.items(), key=lambda x: x[1], reverse=True)
    for word, score in sorted_words[:3]:
        print("\tWord: {}, TF-IDF: {}".format(word, round(score, 5)))

# Export data 
raw_data.to_csv('Proejct_Tags.csv') #Change File name to project name. 
