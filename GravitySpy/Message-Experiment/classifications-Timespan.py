# Authour: Mahboobeh "Mabi" Harandi
# GravitySpy Project iSchool Syracuse Fall 2016



import datetime
import pandas as pd
import csv
import re
import itertools


#this function collect the data in the given range by the user
def timespan(numtimespan):
    reference_date = datetime.datetime(2016, 10, 12, 12, 30)
    start_date = reference_date + datetime.timedelta(-numtimespan)
    end_date = reference_date + datetime.timedelta(numtimespan)
    first_line = True
    classifications = csv.reader(open("Gs_Class_Message_20161206-no-duplicate-SessClass.csv"))
    classifications_rows = [row for row in classifications]
    with open("%s-day(s)-range-launch.csv" %numtimespan, "w") as output:
        theoutput = csv.writer(output)
        for row in classifications_rows:
            if first_line:
                theoutput.writerow(row + ['after'])
                first_line = False
                continue
            found_date = datetime.datetime.strptime(row[29], '%m/%d/%y %H:%M')
            if found_date >= start_date and found_date <= reference_date:
                theoutput.writerow(row + [0])
            if found_date >= reference_date and found_date <= end_date:
                theoutput.writerow(row + [1])
    return ("%s-day(s)-range-launch.csv" %numtimespan)


#this function collect the data in the given range by the user BUT it doesn't accumulate the days and it's only collect data
# after the launch date
def timespan_non_accumulated_afterlaunch(numnonacc):
    reference_date = datetime.datetime(2016, 10, 12, 12, 30)
    end_date_1 = reference_date + datetime.timedelta(numnonacc)
    first_line = True
    classifications = csv.reader(open("Gs_Class_Message_20161206-no-duplicate-SessClass.csv"))
    classifications_rows = [row for row in classifications]
    with open("%s-day(s)-range-launch-non-accumulated.csv" %numnonacc, "w") as output:
        theoutput = csv.writer(output)
        for row in classifications_rows:
            if first_line:
                theoutput.writerow(row + ['week'])
                first_line = False
                continue
            found_date = datetime.datetime.strptime(row[29], '%m/%d/%y %H:%M')
            if found_date >= reference_date and found_date <= end_date_1:
                theoutput.writerow(row + [1])
            if found_date > end_date_1:
                theoutput.writerow(row + [2])
    return ("%s-day(s)-range-launch-non-accumulated.csv" %numnonacc)



#This func writes a file according to the UserId, the message type that they have received, whether before or after the launch date and the nubmer of classification that they have done in each range
def grouping(file_name):
    df1 = pd.read_csv(file_name)
    df2 = df1.groupby(['cohort', 'userID', 'after'])['userID'].size()
    temp = file_name.title()
    m = re.search("(.+?).Csv", temp)
    if m:
        file_name = m.group(1)
    with open("%s-grouped.csv" % file_name, 'w') as output:
        df2.to_csv(output, sep=',')



# This func writes a file according to the UserId, the message type that they have received,
# whether before or after the launch date and the nubmer of classification that they have done in each range
# for each week after the laucnh date
def grouping_non_accumulated(file_name):
    df1 = pd.read_csv(file_name)
    df2 = df1.groupby(['cohort', 'userID', 'week'])['datetime'].max().reset_index()
    df3 = df1.groupby(['cohort', 'userID', 'week'])['userID'].size().reset_index()
    temp = file_name.title()
    m = re.search("(.+?).Csv", temp)
    if m:
        file_name = m.group(1)
    with open("%s-grouped.csv" % file_name, 'w') as output:
        df2.to_csv(output, sep=',')
    with open("%s-grouped-2.csv" % file_name, 'w') as output:
        df3.to_csv(output, sep=',')


#This func gets the number of days for checking range on the reference date
def get_number_of_days():
    try:
        num = int(input('For accumulated version, please enter 1, For non-accumulated version after launch date, please enter 0: '))
        while (num==0 or num ==1):
            if num == 1:
                numtimespan = int(input('Please enter the number of days: '))
                file_name = timespan(numtimespan)
                print('The first file is ready')
                grouping(file_name)
                print('It is done, please check the folder')
            if num == 0:
                numnonacc = int(input('Please enter the number of days for non-accumulated version: '))
                file_name = timespan_non_accumulated_afterlaunch(numnonacc)
                print('The first file is ready')
                grouping_non_accumulated(file_name)
                print('It is done, please check the folder')
        print ('The number is not either 0 or 1!')
    except ValueError:
        print ("Not a number")




# this function calculates the classifications number for each user based on accumulated or non-accumulated version, considering
# the cohort message. In accumulated version, if we ask for 1 day before and after the launch date, then we ask for 30 days before and
# after the launch date, 1 day numbers will be counted in 30 days. But in non-accumulated version, if we choose 7, we will have data
# for the first 7 days and the second 7 days without including the first 7 days in the second one.
# get_number_of_days()


