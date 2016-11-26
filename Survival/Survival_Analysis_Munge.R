library(survival)
library(plyr)
library(reshape)
library(reshape2)


### Functions 
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

shift2 <- function(x, n) `length<-`(tail(x, -n), length(x)) 

### Import dataset

chimp_and_see_20161023 <- read.csv("~/Desktop/Research Projects/Survival (ICWSM)/Data Analysis/chimp_and_see_20161023.csv")
chimp_and_see_20161023$datetime <- as.POSIXct(chimp_and_see_20161023$datetime, format="%Y-%m-%d %H:%M:%S")


############ User Level Variables ############
user <- ddply(chimp_and_see_20161023, c("user_name"), summarise,
	classifications = length(user_name),
	max_session = max(Session),
	first_classification = min(datetime),
	last_classification = max(datetime)
	)

# Select only those users with 2 or more classifications
user_population <- user[which(user$classifications >1), ]
classifications_population <- merge(chimp_and_see_20161023, user_population, by=c("user_name"), all.y=FALSE)


user_session <- ddply(classifications_population, c("user_name","Session"), summarise,
	classifications = length(user_name),
	first_classification = min(datetime),
	last_classification = max(datetime)
	)
user_session$length <- user_session$last_classification - user_session$first_classification


############ Accuracy ############

### Create matrix of subjects and responses
accuracy.presence <- as.data.frame.matrix(table(classifications_population$subject_id,classifications_population$presence))
accuracy.animal <- as.data.frame.matrix(table(classifications_population$subject_id,classifications_population$animal))
accuracy.number <- as.data.frame.matrix(table(classifications_population$subject_id,classifications_population$number))
accuracy.age <- as.data.frame.matrix(table(classifications_population$subject_id,classifications_population$age))
accuracy.sex <- as.data.frame.matrix(table(classifications_population$subject_id,classifications_population$sex))

### Since the subject_ids are the rownames create list to append later
subject_id <- rownames(accuracy.presence)

### Takes row heading (i.e., answer) with the highest value, then combines subject_id, and renames columns
accuracy.presence.answer <- as.data.frame(colnames(accuracy.presence)[max.col(accuracy.presence ,ties.method="first")])
accuracy.presence.answer <- cbind(accuracy.presence.answer,subject_id)
accuracy.presence.answer  <- rename(accuracy.presence.answer, c("colnames(accuracy.presence)[max.col(accuracy.presence, ties.method = \"first\")]" = "crowd_presence"))

accuracy.animal.answer <- as.data.frame(colnames(accuracy.animal)[max.col(accuracy.animal ,ties.method="first")])
accuracy.animal.answer <- cbind(accuracy.animal.answer,subject_id)
accuracy.animal.answer  <- rename(accuracy.animal.answer, c("colnames(accuracy.animal)[max.col(accuracy.animal, ties.method = \"first\")]" = "crowd_animal"))

accuracy.number.answer <- as.data.frame(colnames(accuracy.number)[max.col(accuracy.number ,ties.method="first")])
accuracy.number.answer <- cbind(accuracy.number.answer,subject_id)
accuracy.number.answer  <- rename(accuracy.number.answer, c("colnames(accuracy.number)[max.col(accuracy.number, ties.method = \"first\")]" = "crowd_number"))

accuracy.age.answer <- as.data.frame(colnames(accuracy.age)[max.col(accuracy.age ,ties.method="first")])
accuracy.age.answer <- cbind(accuracy.age.answer,subject_id)
accuracy.age.answer  <- rename(accuracy.age.answer, c("colnames(accuracy.age)[max.col(accuracy.age, ties.method = \"first\")]" = "crowd_age"))

accuracy.sex.answer <- as.data.frame(colnames(accuracy.sex)[max.col(accuracy.sex ,ties.method="first")])
accuracy.sex.answer <- cbind(accuracy.sex.answer,subject_id)
accuracy.sex.answer  <- rename(accuracy.sex.answer, c("colnames(accuracy.sex)[max.col(accuracy.sex, ties.method = \"first\")]" = "crowd_sex"))

crowd.answer <- cbind(accuracy.presence.answer,accuracy.animal.answer,accuracy.number.answer,accuracy.age.answer,accuracy.sex.answer)
remove(accuracy.presence,accuracy.animal,accuracy.number,accuracy.sex,accuracy.age,subject_id,accuracy.presence.answer,accuracy.animal.answer,
	accuracy.number.answer,accuracy.sex.answer,accuracy.age.answer)

crowd.answer$subject_id <- NULL
crowd.answer$subject_id <- NULL
crowd.answer$subject_id <- NULL
crowd.answer$subject_id <- NULL
classifications_population <- merge(classifications_population, crowd.answer, by=c("subject_id"))

### Change Variables
classifications_population$crowd_presence <- mapvalues(classifications_population$crowd_presence, 
	from = c("V1"),
	  to = c("False"))
classifications_population$presence <- mapvalues(classifications_population$presence, 
	from = c("","False"),
	  to = c("False","True"))
classifications_population$presence_a <- ifelse(classifications_population$crowd_presence == classifications_population$presence,1,0)


##
classifications_population$crowd_animal <- mapvalues(classifications_population$crowd_animal, 
	from = c("V1"),
	  to = c("NoAns"))
classifications_population$animal <- mapvalues(classifications_population$animal, 
	from = c(""),
	  to = c("NoAns"))
classifications_population$animal_a <- ifelse(classifications_population$crowd_animal == classifications_population$animal,1,0)
##
classifications_population$crowd_number <- mapvalues(classifications_population$crowd_number, 
	from = c("V1"),
	  to = c("NoAns"))
classifications_population$number <- mapvalues(classifications_population$number, 
	from = c(""),
	  to = c("NoAns"))
classifications_population$number_a <- ifelse(classifications_population$crowd_number == classifications_population$number,1,0)
##
classifications_population$crowd_sex <- mapvalues(classifications_population$crowd_sex, 
	from = c("V1"),
	  to = c("NoAns"))
classifications_population$sex <- mapvalues(classifications_population$sex, 
	from = c(""),
	  to = c("NoAns"))
classifications_population$sex_a <- ifelse(classifications_population$crowd_sex == classifications_population$sex,1,0)
##
classifications_population$crowd_age <- mapvalues(classifications_population$crowd_age, 
	from = c("V1"),
	  to = c("NoAns"))
classifications_population$age <- mapvalues(classifications_population$age, 
	from = c(""),
	  to = c("NoAns"))
classifications_population$age_a <- ifelse(classifications_population$crowd_age == classifications_population$age,1,0)

### Develop composite number for image score based on presence, animal, and number 
classifications_population$accuracy <- (classifications_population$animal_a + classifications_population$number_a + classifications_population$presence_a)/3

############################################################
user_population_extended_session <- ddply(classifications_population, c("user_name","Session"), summarise,
	classifications = length(user_name),
	first_classification = min(datetime),
	last_classification = max(datetime),
	presence_id = sum(presence_a=='1')/length(user_name),
	animal_id = sum(animal_a=='1')/length(user_name),
	number_id = sum(number_a=='1')/length(user_name),
	accuracy = sum(accuracy)/length(user_name),
	sessions = max(Session)
	)
user_population_extended_session$length <- user_population_extended_session$last_classification - user_population_extended_session$first_classification
user_population_extended_session <- merge(user_population_extended_session, user[,c("user_name", "max_session")], by=("user_name"))
user_population_extended_session$futuresession <-  user_population_extended_session$max_session - user_population_extended_session$Session
user_population_extended_session$survive <-  ifelse(user_population_extended_session$futuresession > 0,1,0)
user_population_extended_session$start <- difftime(user_population_extended_session$first_classification, "2015-04-14 14:00:48", units = "mins")
user_population_extended_session$stop <- difftime(user_population_extended_session$last_classification, "2015-04-14 14:00:48", units = "mins")



user_population_extended <- ddply(classifications_population, c("user_name"), summarise,
	classifications = length(user_name),
	first_classification = min(datetime),
	last_classification = max(datetime),
	presence_id = sum(presence_a=='1')/length(user_name),
	animal_id = sum(animal_a=='1')/length(user_name),
	number_id = sum(number_a=='1')/length(user_name),
	accuracy = sum(accuracy)/length(user_name)
	)
user_population_extended$length <- user_population_extended$last_classification - user_population_extended$first_classification




