# Install packages 
install.packages("ggplot2")
install.packages("plyr")
install.packages("reshape2")
install.packages("lubridate")
# Load libraries
library(ggplot2)
library(plyr)
library(reshape2)
library(lubridate)

# Set working directory to same directory where files are exported to in Python code
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
Anon$datetime3 <- strptime(x = as.character(Anon$datetime),
        format = "%Y-%m-%d %H:%M:%S")
Anon$datetime3 <- as.POSIXct(Anon$datetime3, format="%Y-%m-%d %H:%M:%S")
################################################
###########  Project Level Analysis ############
################################################

# Data for table 1 in results on project decriptives
Project_Start <- min(ip_work_Session$Earliest)
Number_Reg_Users <- length(unique(Anon$user_name))
Number_Anon_Users <- length(unique(Anon$user_ip))
Sum_Anon_Annotations <- sum(ip_work$Anon_Annotations)
Sum_Reg_Annotations <- sum(ip_work$Registered)
Sum_Anon_Annotations_Beyond <- sum(ip_work_Session$Anon_Annotations[ip_work_Session$Session > 1]) # Number of anon work after first session

# Anon Work/Reg Work by Month
Contribution_by_YearMonth <- ddply(Anon,
       .( format(datetime3, "%Y%m" )), 
        summarize, 
        CountClassifications=length(Classifications), 
        Anonymous = length(which(is.na(user_name))),
		Registered = length(which(!is.na(user_name))),
		Percentage_Anon = length(which(is.na(user_name)))/length(Classifications),
		Percentage_Reg = length(which(!is.na(user_name)))/length(Classifications)
		)

# Rename heading 
Contribution_by_YearMonth <- rename(Contribution_by_YearMonth,c("format(datetime3, \"%Y%m\")" = "Year"))
Contribution_Type <- Contribution_by_YearMonth[,c(1,3,4)]
# Melt dataset by year variable
Contribution_Type.m  <-melt(Contribution_Type, id.vars = c("Year"))

# Density plots (Need to add percentages and label axis)
p <- ggplot(Contribution_Type.m,aes(x=Year, y=log(value), group=variable, fill=variable,colour = variable))

setwd("~/Dropbox/ZooSOCS dropbox/Papers/CSCW 2017 (AnonWork)/Anon-LaTex/figs")
pdf("higgs_history.pdf", height=5, width=10) # Need to change plot name
ggplot(Contribution_Type.m) +
    aes(Year, value, group=variable,fill=variable,colour=variable) +
    stat_summary(fun.y="sum",geom="density",alpha=0.6) +
    ggtitle(project) +
    theme(
        legend.position="none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size=12)	
    )
dev.off()

# Data Summaries
library(psych)
describe.by(ip_work)
describe.by(ip_work_Session)

#############################################
########## Session Level Analysis ###########
#############################################

###### Calculate user history work by session ######
ip_work_Session = ddply(Anon, c("user_ip","Session"), summarize,
	Annotations = length(user_name),
	Anon_Annotations = length(which(is.na(user_name))),
	Registered = length(which(!is.na(user_name))),
	Earliest = min(datetime,na.rm=TRUE),
	Recent = max(datetime,na.rm=TRUE)
	)

ip_work_Session$Earliest <- as.POSIXct(ip_work_Session$Earliest, format="%Y-%m-%d %H:%M:%S")
ip_work_Session$Recent <- as.POSIXct(ip_work_Session$Recent, format="%Y-%m-%d %H:%M:%S")
ip_work_Session$Time <- ip_work_Session$Recent- ip_work_Session$Earliest
# Narrow data to "interesting" sessions to examine. Those who's session arent at either extreme
ip_work_Session$user_ip <- with(ip_work_Session, reorder(user_ip, Session))
ip_work_Session$Portion <- ip_work_Session$Anon_Annotations/ip_work_Session$Annotations
ip_work_Session_Mix <- ip_work_Session[which(ip_work_Session$Portion < 1 & ip_work_Session$Portion > 0),]
ip_work_Session_Mix_After <- ip_work_Session_Mix[which(ip_work_Session_Mix$Session > 1),]
ip_work_Session_Mix$Project <- project

ip_work_Session_AnonOnly <- ip_work_Session[which(ip_work_Session$Portion == 1),]
ip_work_Session_RegOnly <- ip_work_Session[which(ip_work_Session$Portion == 0 ),]

# Subset for users not in their first session

# Export Sessions for analysis and examination
setwd("~/Desktop/Sequence/SmallSessions")
write.csv(ip_work_Session_Mix,"MixHiggs.csv")

# Grab annotations of those in Mix[PROJECT] for sequence analysis 
library(sqldf)
ip_work_Session_Mix_Anon_H <-  sqldf("SELECT user_ip,user_name,Session,Classifications,Time_Seconds FROM Anon_Annotations WHERE EXISTS(SELECT 1 FROM ip_work_Session_Mix WHERE Anon_Annotations.user_ip = ip_work_Session_Mix.user_ip AND Anon_Annotations.Session = ip_work_Session_Mix.Session)
")

setwd("~/Desktop/Sequence/SmallSessions")
write.csv(ip_work_Session_Mix_Anon_H,"MixHiggsAnnotations.csv")

# Scatterpolot for Anonymous Annotations and Portion done in session
library(ggplot2)
p <- ggplot(ip_work_Session, aes(Portion, Anon_Annotations))
p + geom_point(aes(size = Annotations) +
theme(
        axis.text = element_text(size=12)	
	)
)

g <- ggplot(ip_work_Session, aes(Registered, Anon_Annotations))
g + geom_point(aes(colour=Portion))+
scale_colour_gradient(low = "blue")+
theme(
        axis.text = element_text(size=12)	
	)
#############################################
############ User Level Analysis ############
#############################################

###### Calculate user history ######
ip_work = ddply(Anon, c("user_ip"), summarize,
	Annotations = length(user_name),
	sessions=max(Session),
	Anon_Annotations = length(which(is.na(user_name))),
	Registered = length(which(!is.na(user_name))),
	Proportion_Anon = length(which(is.na(user_name)))/length(user_name),
	Earliest = min(datetime),
	Recent = max(datetime)
	)

ip_work$Earliest <- as.POSIXct(ip_work$Earliest, format="%Y-%m-%d %H:%M:%S")
ip_work$Recent <- as.POSIXct(ip_work$Recent, format="%Y-%m-%d %H:%M:%S")
ip_work$Membership <- ip_work$Recent-ip_work$Earliest

# Able to determine source of anonymous contributions
ip_work_ASSO = ddply(Anon, c("user_ip"), summarize,
	Unique_Users = length(unique(user_name)),
	Anon_Annotations = length(which(is.na(user_name)))
	)

ip_Mix = ddply(Anon, c("user_ip"), summarize,
	Unique_Users = length(unique(sessions))
	)

# Above gives us a count of unique user names associated with each user ip. Since the unique ip can include
#	cases where the only obsevation was NA or a user name we need to further subset the data to cases where
#	unique_users > 1 and anon_Annotations > 0
ip_work_ASSO <- ip_work_ASSO[which(ip_work_ASSO$Unique_Users > 1 & ip_work_ASSO$Anon_Annotations > 0 ),]

# Subset for users who only contributed registered
ip_work_AnonOnly <- ip_work[which(ip_work$Proportion_Anon == 1),]
ip_work_RegOnly <- ip_work[which(ip_work$Proportion_Anon == 0 ),]
ip_work_Mix <- ip_work[which(ip_work$Anon_Annotations < 1 &  ip_work$Registered > 0),]

#http://www.r-bloggers.com/visualizing-the-history-of-epidemics/
#https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
#http://www.r-bloggers.com/ggplot2-quick-heatmap-plotting/

ip_work_Session$Type[ip_work_Session$Portion == 1 ] <- "Identified"
ip_work_Session$Type[ip_work_Session$Portion == 0  ] <- "Anonymous"
ip_work_Session$Type[ip_work_Session$Portion < 1 &  ip_work_Session$Portion > 0] <- "Mix"



# Get dedicated users 
ip_work_Typologies = ddply(ip_work_Session, c("user_ip"), summarize,
	Sessions = length(user_ip),
	Identified = sum(Type == 'Identified'),
	Anonymous = sum(Type == 'Anonymous'),
	Mix = sum(Type == 'Mix'),
	Annotations = sum(Annotations),
	Anon_Annotations = sum(Anon_Annotations),
	Identified_Annotations = sum(Registered),
	Time = sum(Time)/length(user_ip)
	)

ip_work_Typologies$Typology[ip_work_Typologies$Identified >= 1 & ip_work_Typologies$Anonymous == 0 & ip_work_Typologies$Mix == 0] <- "Dedicated Identified"
ip_work_Typologies$Typology[ip_work_Typologies$Identified == 0 & ip_work_Typologies$Anonymous >= 1 & ip_work_Typologies$Mix == 0] <- "Dedicated Anonymous"
ip_work_Typologies$Typology[ip_work_Typologies$Identified == 0 & ip_work_Typologies$Anonymous == 0 & ip_work_Typologies$Mix >= 1] <- "Dedicated Mix"
ip_work_Typologies$Typology[ip_work_Typologies$Identified >= 1 & ip_work_Typologies$Anonymous >= 1 & ip_work_Typologies$Mix == 0] <- "Togglers"
ip_work_Typologies$Typology[ip_work_Typologies$Identified == 0 & ip_work_Typologies$Anonymous >= 1 & ip_work_Typologies$Mix >= 1] <- "Togglers"
ip_work_Typologies$Typology[ip_work_Typologies$Identified >= 1 & ip_work_Typologies$Anonymous == 0 & ip_work_Typologies$Mix >= 1] <- "Togglers"
ip_work_Typologies$Typology[ip_work_Typologies$Identified >= 1 & ip_work_Typologies$Anonymous >= 1 & ip_work_Typologies$Mix >= 1] <- "Rangers"


# Subset for more than 1 session
ip_work_Typologies_Sustained <- ip_work_Typologies[which(ip_work_Typologies$Sessions > 1),]

#table(ip_work_Typologies$Typology)
#table(ip_work_Typologies_Sustained$Typology)


library(psych)
describe.by(ip_work_Typologies, ip_work_Typologies$Typology)

