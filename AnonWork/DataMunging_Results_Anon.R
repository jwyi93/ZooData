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
setwd("~PATH TO FOLDER")

# Import Dataset from .csv file
Anon <- read.csv("")
project = Asteroid Zoo # Change Name to current project

############ Data Munging ############
# Change blank fields in user_name to NA
Anon$user_name[Anon$user_name ==""]  <- NA 
# Change datetime to a datetime variable R can recognize

################################################
#											   #
#			Project Level Analysis 		       #
#											   #
################################################

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
# Subset
Contribution_Type <- Contribution_by_YearMonth[,c(1,3,4)]
# Melt dataset
Contribution_Type.m  <-melt(Contribution_Type, id.vars = c("Year"))

# Density plots
p <- ggplot(Contribution_Type.m, 
	aes(x=Year, y=log(value), group=variable, fill=variable,colour = variable))
Contribution = 
p + geom_line() + 
	ggtitle(project) +
	theme(
	legend.position="none",
	axis.title.y=element_blank(),
	axis.title.x=element_blank(),
	axis.text.x = element_text(angle=34,size=10)	
	)

# Plot of contribution type by month
pdf("NAME.pdf", height=5, width=10) # Need to change plot name
# Plot
dev.off()

############ User Level Analysis ############
start_date = min(ip_work$Earliest, na.rm=TRUE)
Anon$datetime <- as.POSIXct(Anon$datetime, format="%Y-%m-%d %H:%M:%S")

# Calculate user history work by session
ip_work_Session = ddply(Anon, c("user_ip","Session"), summarize,
	Annotations = length(user_name),
	sessions=max(Session),
	Anon_Annotations = length(which(is.na(user_name))),
	Registered = length(which(!is.na(user_name))),
	Earliest = min(datetime),
	Recent = max(datetime),
	Session_Length = (max(datetime) - min(datetime))
	)

ip_work_Session$Earliest <- as.POSIXct(ip_work_Session$Earliest, format="%Y-%m-%d %H:%M:%S")
ip_work_Session$Recent <- as.POSIXct(ip_work_Session$Recent, format="%Y-%m-%d %H:%M:%S")

# Calculate user history
ip_work = ddply(Anon, c("user_ip"), summarize,
	Annotations = length(user_name),
	sessions=max(Session),
	Anon_Annotations = length(which(is.na(user_name))),
	Registered = length(which(!is.na(user_name))),
	Earliest = min(datetime),
	Recent = max(datetime),
	Seniority = (max(datetime) - min(datetime)),
	Joined_After = as.difftime(min(datetime) - start_date, unit=days)
	)

ip_work$Earliest <- as.POSIXct(ip_work$Earliest, format="%Y-%m-%d %H:%M:%S")
ip_work$Recent <- as.POSIXct(ip_work$Recent, format="%Y-%m-%d %H:%M:%S")
ip_work$Membership <- difftime(ip_work$Earliest-ip_work$Recent)

# Calculate Time
Anon$datetime3 <- strptime(x = as.character(Anon$datetime),
        format = "%Y-%m-%d %H:%M:%S")

# Export Plot
pdf("NAME.pdf", height=5, width=10) # Need to change plot name
#Plot
dev.off()

# Remove all the files we created since we have the data we need. 
#remove()
