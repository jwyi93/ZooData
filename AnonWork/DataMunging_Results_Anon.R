# Install packages to be used in this analysis 
install.packages("ggplot2")
install.packages("plyr")
install.packages("reshape2")

# Load libraries to current session
library(ggplot2)
library(plyr)
library(reshape2)

# Set working directory to same directory where files are exported to in Python code
setwd("~/Dropbox/ZooSOCS dropbox/Papers/CSCW 2017 (AnonWork)/RawData/DataFiles")

# Import Dataset
Higgs_Anon <- read.csv("~/Dropbox/ZooSOCS dropbox/Papers/CSCW 2017 (AnonWork)/RawData/DataFiles/Higgs_Anon.csv") # Might take some time
project = Higgs Hunters

############ Data Munging ############
# Change blank fields in user_name to NA
Higgs_Anon$user_name[Higgs_Anon$user_name ==""]  <- NA 
# Change datetime to a datetime variable R can recognize
Higgs_Anon$datetime3 <- strptime(x = as.character(Higgs_Anon$datetime),
                                 format = "%Y-%m-%d %H:%M:%S")


################################################
#											   #
#			Project Level Analysis 		       #
#											   #
#											   #
################################################

# Anon Work/Reg Work by Month
Contribution_by_YearMonth <- ddply(Higgs_Anon,
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
remove(Contribution_Type)
# Density plots
p <- ggplot(Contribution_Type.m, 
	aes(x=Year, y=log(value), group=variable, fill=variable,colour = variable))
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
ip_work = ddply(Higgs_Anon, c("user_ip"), summarize, 
	annotations = length(user_ip),
	sessions=max(Session),
	Anon_Annotations = length(which(is.na(user_name))),
	Registered = length(which(!is.na(user_name))))

# Export Plot
pdf("NAME.pdf", height=5, width=10) # Need to change plot name
#Plot
dev.off()

# Remove all the files we created since we have the data we need. 
#remove()
