#### Remove duplicates from file
def no_duplicate(filename):
   df1 = pd.read_csv(filename)
   df2 = df1.drop_duplicates(subset= ['userID'])
   temp = filename.title()
   m = re.search("(.+?).Csv", temp)
   if m:
       file_name = m.group(1)
   with open('%s-no-duplicate.csv' %file_name, 'w') as output:
       df2.to_csv(output, sep=',')

filename = "classifications_with_timestamp.csv"
no_duplicate(filename)
