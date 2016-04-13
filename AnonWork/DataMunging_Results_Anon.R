# Install packages 
install.packages("ggplot2")
install.packages("plyr")
install.packages("reshape2")

# Load libraries
library(ggplot2)
library(plyr)
library(reshape2)

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

# Summary Information by user
ip_work_Session = ddply(Anon, c("user_ip","Session"), summarize,
	Annotations = length(user_name),
	sessions=max(Session),
	Anon_Annotations = length(which(is.na(user_name))),
	Registered = length(which(!is.na(user_name)))
	)

ip_work = ddply(Anon, c("user_ip"), summarize,
	Annotations = length(user_name),
	sessions=max(Session),
	Anon_Annotations = length(which(is.na(user_name))),
	Registered = length(which(!is.na(user_name)))
	)

# Calculate Time
Anon$datetime3 <- strptime(x = as.character(Anon$datetime),
        format = "%Y-%m-%d %H:%M:%S")

# Export Plot
pdf("NAME.pdf", height=5, width=10) # Need to change plot name
#Plot
dev.off()

# Remove all the files we created since we have the data we need. 
#remove()
