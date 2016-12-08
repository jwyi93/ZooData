# Corey Jackson
# Syracuse University 2016

# Merge three files 
# 1. gravity.spy.experiment with experiment assignment (login name no userID though)
# 2. GS_id_login has login name and userID
# 3. class_GS has gravity spy classifications with userID

# Merge files
users <- merge(GS_id_login, gravity.spy.experiment, by=c("login"), all.x = TRUE)
classifications <- merge(class_GS, users, by.x=c("userID"), by.y=c("id"), all.x = TRUE)

# export new data
write.csv(classifications, "class_w_dem.csv")
