library(plyr)

# Anonymous Sessions Profiling 
# Taken from file generated in DataAnalysis.R code (write.csv(ip_work_Session_Mix,"Mix[PROJECT_NAME].csv"))

# Set working directory
setwd("~PATH TO FOLDER")

# Import projects
ProjectName <- read.csv("")
ProjectName <- read.csv("")
ProjectName <- read.csv("")
ProjectName <- read.csv("")
ProjectName <- read.csv("")

AllAnonMix <- rbind(ProjectName,ProjectName,ProjectName,ProjectName,ProjectName)
# Summary stats for all projects



###### K-means clustering for session types ######
ip_work_Session <- na.omit(ip_work_Session)
ip_work_Session.Clusters <- ip_work_Session[c(4,5)]
# Determine number of clusters 
wss <- (nrow(ip_work_Session.Clusters)-1)*sum(apply(ip_work_Session.Clusters,2,var))

# When the chart appears you need to find the sharpest increse and make that the number of clusters
for (i in 2:15) wss[i] <- sum(kmeans(ip_work_Session.Clusters, 
    centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")

# K-Means Cluster Analysis. Review elbow plot to determine clusters
fit <- kmeans(ip_work_Session.Clusters, 4) # Change variable here to fit number of clusters
# Mean work for cluster assignments
aggregate(ip_work_Session.Clusters,by=list(fit$cluster),FUN=mean)
# Append cluster assignment to data
ip_work_Session<- data.frame(ip_work_Session, fit$cluster)

###### End K-means clustering ######