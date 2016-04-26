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

# Change session to factor
Anon$Session <- as.factor(Anon$Session)
# Add variable to determine if observation falls within session range i.e., less than 30 minutes and greater than 1
Anon$Time_Include <- ifelse(Anon$Time_Seconds <= 1799 & Anon$Time_Seconds >= 1,1,0)
Anon_Annotations <- Anon[which(Anon$Time_Include == 1),]

###########  Project Level Analysis ############
#											   #
################################################

# Anon Work/Reg Work by Month
Contribution_by_YearMonth <- ddply(Anon_Annotations,
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

# Density plots
 <- ggplot(Contribution_Type.m, 
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

########## Session Level Analysis ###########
#										    #
#############################################

# Calculate user history work by session
ip_work_Session = ddply(Anon_Annotations, c("user_ip","Session"), summarize,
	Annotations = length(user_name),
	Anon_Annotations = length(which(is.na(user_name))),
	Registered = length(which(!is.na(user_name))),
	Earliest = min(datetime),
	Recent = max(datetime),
	Time = sum(Time_Seconds)
	)

ip_work_Session$Earliest <- as.POSIXct(ip_work_Session$Earliest, format="%Y-%m-%d %H:%M:%S")
ip_work_Session$Recent <- as.POSIXct(ip_work_Session$Recent, format="%Y-%m-%d %H:%M:%S")


# K-means clustering for session types
ip_work_Session <- na.omit(ip_work_Session)
ip_work_Session.Clusters <- ip_work_Session.C[c(4,5)]
# Determine number of clusters 
wss <- (nrow(ip_work_Session.Clusters)-1)*sum(apply(ip_work_Session.Clusters,2,var))

# When the chart appears you need to find the sharpest increse and make that the number of clusters
for (i in 2:15) wss[i] <- sum(kmeans(ip_work_Session.Clusters, 
    centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")

# K-Means Cluster Analysis. Review elbow plot to determine clusters
fit <- kmeans(ip_work_Session.Clusters, 4)
# Cluster means 
aggregate(ip_work_Session.Clusters,by=list(fit$cluster),FUN=mean)
# append cluster assignment to data
ip_work_Session<- data.frame(ip_work_Session, fit$cluster)

##### Plot for Session and Proportion Anonymous

# Subset to look at no more than 20 sessions
# removes puncutation characters in text
# ip_work_Session_Sub$user_ip <- gsub("[[:punct:]]", "", ip_work_Session_Sub$user_ip)





ip_work_Session_Sub <- ip_work_Session[which(ip_work_Session$Session > 1 & ip_work_Session$Session < 10),]
ip_work_Session_Sub$Portion <- ip_work_Session_Sub$Anon_Annotations/ip_work_Session_Sub$Annotations
ip_work_Session_Sub$user_ip <- with(ip_work_Session_Sub, reorder(user_ip, Portion))

# Capture only Proportion Variable 
ip_work_Session_Sub.m <- ip_work_Session_Sub[,c(1,2,9)]
ip_Session_Sub.m <- ip_work_Session_Sub.m[1:2000,]
# Scale by session number (not necessary)
#ip_work_Session_Sub.m <- ddply(ip_work_Session_Sub.m, .(Session), transform,rescale = scale(Portion))

# Plot
Session <- ggplot(ip_Session_Sub.m, aes(Session, user_ip)) + 
	geom_tile(aes(fill = Portion),colour = "white") + 
	scale_fill_gradient(low = "grey",high = "black") + 
	theme (
		axis.text.y = element_blank(),
		axis.ticks.y = element_blank()
		)


############ User Level Analysis ############
#										    #
#############################################



# Calculate user history
ip_work = ddply(Anon, c("user_ip"), summarize,
	Annotations = length(user_name),
	sessions=max(Session),
	Anon_Annotations = length(which(is.na(user_name))),
	Registered = length(which(!is.na(user_name))),
	Proportion_Anon = length(which(is.na(user_name)))/length(user_name),
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





http://www.r-bloggers.com/visualizing-the-history-of-epidemics/
https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
http://www.r-bloggers.com/ggplot2-quick-heatmap-plotting/
