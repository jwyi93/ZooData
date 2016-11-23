# Corey Jackson
# Syracuse University 2016


## Promotion Analysis for Gravity Spy
# After users are promoted do they still do analysis in other workflows


library(plyr)

# Generate file with user workflow name and promotion date
promotions <- ddply(classifications, c("userID","workflow"),
	Date = min(datetime))






