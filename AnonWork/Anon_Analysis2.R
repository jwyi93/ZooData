# Code for analyzing Zooniverse anonymous work data 
# Corey Jackson, School of Information Studies, Syracuse University, July 2016


# Install packages 
install.packages("ggplot2")
install.packages("plyr")
install.packages("reshape2")
install.packages("lubridate")
install.packages("sqldf")
install.packages("hexbin")
# Load libraries
library(ggplot2)
library(plyr)
library(reshape2)
library(lubridate)
library(sqldf)
library(hexbin)


# Functions 
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}

#### Import Script
setwd("~/Dropbox/ZooSOCS dropbox/Papers/CSCW 2017 (AnonWork)/RawData/DataFiles")

# Import Dataset from .csv file
Anon <- read.csv("HiggsHunters.csv")
project = Higgs Hunters # Change Name to current project

############ Data Munging ############
Anon$user_name[Anon$user_name ==""]  <- NA 
Anon$datetime <- as.POSIXct(Anon$datetime, format="%Y-%m-%d %H:%M:%S")
Anon$Time_Include <- ifelse(Anon$Time_Seconds <= 1799 & Anon$Time_Seconds >= 1,1,0) # Add variable to determine if observation falls within session range i.e., less than 30 minutes and greater than 1

Anon_Annotations <- Anon[which(Anon$Time_Include == 1),]
Anon_Annotations$datetime <- as.POSIXct(Anon_Annotations$datetime, format="%Y-%m-%d %H:%M:%S")
Anon_Annotations <- completeFun(Anon_Annotations, "datetime")
Anon_Annotations$datetime3 <- strptime(x = as.character(Anon_Annotations$datetime),
        format = "%Y-%m-%d %H:%M:%S")
Anon_Annotations_Higgs$datetime3 <- as.POSIXct(Anon_Annotations_Higgs$datetime3, format="%Y-%m-%d %H:%M:%S")


####################################################################################
# Number of anonymous, ambiguious, and registered annotations, users, ips

user_name_ip = ddply(Anon_Annotations_Higgs, c("user_name"), summarize,
	ips = length(unique(user_ip))
	)

### 761 of 6355 usernames ambiguious (e.g., ambiguious) 
### 17213 unknown usernames (e.g., anonymous)
### 5577 user ips known (e.g., identified)
### 22507 total unique ips

# Subset identified user's work. Ips for these users needed. 
userstoselect <- sqldf("Select distinct(user_ip) from Anon_Annotations_Higgs where user_name in (select user_name from user_name_ip where ips == 1)")
Anon_Identified <- sqldf("Select * from Anon_Annotations_Higgs where user_ip in (Select user_ip from userstoselect)")
remove(userstoselect)

### 367323 annotations we can associate with a user name
### Annotations we can actually count  
Anon_Identified <- Anon_Identified[which(Anon_Identified$same_ip == "True"),]

### 361843

# Userip work for identified
ip_work_Session = ddply(Anon_Identified, c("user_ip","Session"), summarize,
	Annotations = length(user_name),
	Anon_Annotations = length(which(is.na(user_name))),
	Registered = length(which(!is.na(user_name))),
	Earliest = min(datetime3,na.rm=TRUE),
	Recent = max(datetime3,na.rm=TRUE)
	)
ip_work_Session$Time <- (ip_work_Session$Recent- ip_work_Session$Earliest)

# Subset of user first sessions
Anon_Identified_First <-  Anon_Identified[which(Anon_Identified$Session == 1),]
ip_work_Session_first = ddply(Anon_Identified_First, c("user_ip","Session"), summarize,
	Annotations = length(user_name),
	Anon_Annotations = length(which(is.na(user_name))),
	Registered = length(which(!is.na(user_name))),
	Earliest = min(datetime3,na.rm=TRUE),
	Recent = max(datetime3,na.rm=TRUE)
	)
ip_work_Session_first$Time <- (ip_work_Session_first$Recent- ip_work_Session_first$Earliest)

User_History = ddply(Anon_Identified, c("user_ip"), summarize,
	Total_Annotations = length(user_name),
	Total_sessions=max(Session),
	Total_Anon_Annotations = length(which(is.na(user_name))),
	Total_Registered = length(which(!is.na(user_name))),
	Total_Earliest = min(datetime3),
	Total_Recent = max(datetime3)
	)
User_History$Total_Time <- (User_History$Total_Recent- User_History$Total_Earliest)

# Combine First sessions and hull history
Anon_Identified_First_Future <- merge(x = ip_work_Session_first, y = User_History, 
	by="user_ip",
	all.x=TRUE)

Anon_Identified_First_Future$FutureSession[Anon_Identified_First_Future$Total_sessions >1] <- 1
Anon_Identified_First_Future$Time <- as.numeric(Anon_Identified_First_Future$Time)
Anon_Identified_First_Future$Total_Time <- as.numeric(Anon_Identified_First_Future$Total_Time)

# Logit Regression for Identified Sessions
Anon_Identified_First_Future$FutureSession <- factor(Anon_Identified_First_Future$FutureSession)
FutureSession_logit <- glm(FutureSession ~ Annotations + Anon_Annotations + Time, data = Anon_Identified_First_Future, family = "binomial")
summary(FutureSession_logit)
exp(coef(FutureSession_logit))

# See what the probability is with averages and increases in anon annotations
# We can do something very similar to create a table of predicted probabilities varying the value of gre and 
# rank. We are going to plot these, so we will create 100 values of anon_Annotations between 1 and 32, 
# at each value of rank (i.e., 1, 2, 3, and 4).

pred_anon1 <- with(Anon_Identified_First_Future,
  data.frame(Anon_Annotations = rep(seq(from = 1, to = 32, length.out = 100), 4),
  Annotations = mean(Annotations), Time = mean(Time)))
## RESULTS 1 in results document
pred_anon2 <- cbind(pred_anon1, predict(FutureSession_logit, newdata = pred_anon1, type="link", se=TRUE))
pred_anon2 <- within(pred_anon2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

setwd("/Users/coreyjackson/Desktop/Anonymous_Work_CHI2017/figs")
pdf("anon_predicted_probability.pdf", height=5, width=10) # Need to change plot name
ggplot(pred_anon2, aes(x = Anon_Annotations, y = PredictedProb)) +
geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .2) +
labs(x="Anonymous Annotations in First Session",y="Predicted Probability of Additional Session(s)")+
	geom_line() +
	theme(
		axis.text = element_text(size=10),
		axis.title = element_text(size =12)
		)
dev.off()
remove(pred_anon1,pred_anon2)


FutureSession_TotalTime <- lm(Total_Time ~ Anon_Annotations + Annotations + Time, data=Anon_Identified_First_Future)
summary(FutureSession_TotalTime)
## RESULTS 2 in results document

FutureSession_TotalSessions <- lm(Total_sessions ~ Anon_Annotations + Annotations + Time, data=Anon_Identified_First_Future)
summary(FutureSession_TotalSessions)
## RESULTS 2 in results document

# New variables 
Anon_Identified_First_Future$Additional_Annotations <- (Anon_Identified_First_Future$Total_Annotations-Anon_Identified_First_Future$Annotations)
Anon_Identified_First_Future$Start_Day <- strptime(x = as.character(Anon_Identified_First_Future$Earliest),format = "%Y-%m-%d")
Anon_Identified_First_Future$Cohort <- round(difftime(Anon_Identified_First_Future$Start_Day,'2014-11-18',unit="days"),digits=0)


FutureSession_TotalAnnotations <- lm(Additional_Annotations ~ Anon_Annotations + Annotations + Time, data=Anon_Identified_First_Future)
summary(FutureSession_TotalAnnotations)
## RESULTS 2 in results document


Anon_Identified_First_Future$Annotation_Mean <- Anon_Identified_First_Future$Time/Anon_Identified_First_Future$Annotations
pdf("Annotation_average_persist.pdf", height=10, width=10) # Need to change plot name
ggplot(Anon_Identified_First_Future, aes(x=Annotations, y=log(Annotation_Mean), colour=FutureSession)) + 
    geom_point() +
    labs(x="Session Annotations",y="Log(Time) in First Session",colour = "Additional Session")+
    theme (
    	axis.text = element_text(size=10),
		axis.title = element_text(size =12)
    	) + 
    geom_smooth(se = FALSE, method = "lm")
dev.off()


#### New RQ: Significant differences in work by continuing/non-continuing users. 
Future.model.sessions <- glm(FutureSession ~ Anon_Annotations+Time+Registered+Cohort, data=Anon_Identified_First_Future, family="binomial")
anova(Future.model, test="Chisq")
summary(Future.model.sessions)
## RESULTS 3 in results document




