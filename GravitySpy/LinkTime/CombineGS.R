# Corey Jackson
# Syracuse University, 2016

# Merging Scotty's classification file with Zooniverse dataset. Timestamps are missing from Scotty's data. 

gravity.spy.classifications_1 <- read.csv("~/Desktop/Research Projects/MessagingExperiment/gravity-spy-classifications_1.csv")
classifications_metadata <- read.csv("~/Downloads/classifications_metadata.csv")

gs_classifications12216 <- merge(gravity.spy.classifications_1,classifications_metadata, by.x=("classificiation_id"), by.y=("classificationID"), all.x=FALSE)
