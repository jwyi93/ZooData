# Authour: Mahboobeh "Mabi" Harandi
# GravitySpy Project iSchool Syracuse Fall 2016

import datetime
import pandas as pd
import csv
import re


#this function collect the data in the given range by the user
def timespan(num):
    reference_date = datetime.datetime(2016, 10, 12, 12, 30)
    start_date = reference_date + datetime.timedelta(-num)
    end_date = reference_date + datetime.timedelta(num)
    first_line = True
    classifications = csv.reader(open("class_w_dem.csv"))
    classifications_rows = [row for row in classifications]
    with open("%s-day(s)-range-launch.csv" %num, "w") as output:
        theoutput = csv.writer(output)
        for row in classifications_rows:
            if first_line:
                theoutput.writerow(row + ['after'])
                first_line = False
                continue
            found_date = datetime.datetime.strptime(row[23], '%m/%d/%y %H:%M')
            if found_date >= start_date and found_date <= reference_date:
                theoutput.writerow(row + [0])
            if found_date >= reference_date and found_date <= end_date:
                theoutput.writerow(row + [1])
    return ("%s-day(s)-range-launch.csv" %num)



#This func writes a file according to the UserId, the message type that they have received, whether before or after the launch date and the nubmer of classification that they have done in each range
def grouping(file_name):
    df1 = pd.read_csv(file_name)
    df2 = df1.groupby(['cohort', 'userID', 'after'])['Classifications'].max().reset_index()
    temp = file_name.title()
    m = re.search("(.+?).Csv", temp)
    if m:
        file_name = m.group(1)
    with open("%s-grouped.csv" % file_name, 'w') as output:
        df2.to_csv(output, sep=',')


#This func gets the number of days for checking range on the referebce date
def get_number_of_days():
    try:
        num = int(input('How many days are you looking for?'))
    except ValueError:
        print ("Not a number")
    return (num)


num = get_number_of_days()
file_name = timespan(num)
grouping(file_name)
