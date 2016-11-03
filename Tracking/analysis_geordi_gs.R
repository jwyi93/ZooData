library(survival)
library(plyr)
library(reshape)
library(reshape2)

# Import Data 
geordi_gs <- read.csv("~/Desktop/Research Projects/{filename}.csv")
geordi_gs$datetime <- as.POSIXct(chimp_and_see_20161023$datetime, format="%Y-%m-%d %H:%M:%S")
