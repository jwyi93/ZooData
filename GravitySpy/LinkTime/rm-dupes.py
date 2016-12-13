# Mabi Harandi
# Syracuse University, 2016

import pandas as pd
import re

def no_duplicate(filename):
    df1 = pd.read_csv(filename)
    df2 = df1.drop_duplicates(subset= ['userID', 'timeToClass'])
    temp = filename.title()
    m = re.search("(.+?).Csv", temp)
    if m:
        file_name = m.group(1)
    with open('%s-no-duplicate.csv' %file_name, 'w') as output:
        df2.to_csv(output, sep=',')

#filename = "classifications_with_timestamp.csv"
filename = "gs_class_message_20161206.csv"
no_duplicate(filename)
