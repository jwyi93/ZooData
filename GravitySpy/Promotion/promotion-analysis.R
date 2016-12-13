# Corey Jackson
# Syracuse University 2016
# File 1 of 2


## Promotion Analysis for Gravity Spy
# After users are promoted do they still do analysis in other workflows

classifications <- read.csv("~/Dropbox/INSPIRE/Data/System dumps/Classifications_GS/Classifications_2016_11-no-duplicate.csv")

library(plyr)
library(reshape2)
library(ggplot2)

classifications$Date <- as.POSIXct(as.character(classifications$datetime),format="%m/%d/%y %H:%M") 

# Generate file with user workflow name and promotion date
promotions <- ddply(classifications, c("userID","workflow"),summarise,
	Date = min(Date),
	Classification = min(Classifications),
	Session = min(Session))

## Build promotion tables and merge promotino tables with classification table

#1610: Neutron Star Mountain
#1934: Galactic Supernova
#1935: Neutron Star Merger
#2360: Black Hole Merger
#2117: Universe Cosmic Background

promotions1610 <- promotions[ which(promotions$workflow==1610), ]
promotions1610 <- rename(promotions1610, replace = c("Date" = "promotion1610"))
classifications <- merge(classifications,promotions1610[, c("userID","promotion1610")], 
	by=("userID"), all.x=TRUE)

promotions1934 <- promotions[ which(promotions$workflow==1934), ]
promotions1934 <- rename(promotions1934, replace = c("Date" = "promotion1934"))
classifications <- merge(classifications,promotions1934[, c("userID","promotion1934")], 
	by=("userID"), all.x=TRUE)

promotions1935 <- promotions[ which(promotions$workflow==1935), ]
promotions1935 <- rename(promotions1935, replace = c("Date" = "promotion1935"))
classifications <- merge(classifications,promotions1935[, c("userID","promotion1935")], 
	by=("userID"), all.x=TRUE)

promotions2360 <- promotions[ which(promotions$workflow==2360), ]
promotions2360 <- rename(promotions2360, replace = c("Date" = "promotion2360"))
classifications <- merge(classifications,promotions2360[, c("userID","promotion2360")], 
	by=("userID"), all.x=TRUE)

promotions2117 <- promotions[ which(promotions$workflow==2117), ]
promotions2117 <- rename(promotions2117, replace = c("Date" = "promotion2117"))
classifications <- merge(classifications,promotions2117[, c("userID","promotion2117")], 
	by=("userID"), all.x=TRUE)

#### Code wouldn't work because of dates so exported to Excel to get level user was in when annotation submitted  !!!!!!!
write.csv(classifications, "classifications_promotion.csv")
#=IF(AE2>=AP2,"Universe Cosmic Background",IF(AE2>=AO2,"Black Hole Merger",IF(AE2>=AN2,"Neutron Star Merger",IF(AE2>=AM2,"Galactic Supernova",IF(AE2>=AL2,"Neutron Star Mountain",1)))))


## Not working. Date Error
#classifications$UserLevel <- ifelse(as.POSIXct(classifications$datetime) >= as.POSIXct(classifications$promotion2117),"Universe Cosmic Background",
#       ifelse(as.POSIXct(classifications$datetime) >= as.POSIXct(classifications$promotion2360), "Black Hole Merger",
#            ifelse(as.POSIXct(classifications$datetime) >= as.POSIXct(classifications$promotions1935), "Neutron Star Merger",
#                ifelse(as.POSIXct(classifications$datetime) >= as.POSIXct(classifications$promotions1934), "Galactic Supernova","Neutron Star Mountain"))))

# Re-import file with userlevel
classifications_promotion <- read.csv("~/Dropbox/INSPIRE/Data/Promotion Analysis/classifications_promotion.csv")
classifications_promotion$workflow <- as.factor(classifications_promotion$workflow)

# Recode new Variables
classifications_promotion$SubmitLevel[classifications_promotion$workflow == "1610"] <- "Neutron Star Mountain"
classifications_promotion$SubmitLevel[classifications_promotion$workflow == "1934"] <- "Galactic Supernova"
classifications_promotion$SubmitLevel[classifications_promotion$workflow == "1935"] <- "Neutron Star Merger"
classifications_promotion$SubmitLevel[classifications_promotion$workflow == "2360"] <- "Black Hole Merger"
classifications_promotion$SubmitLevel[classifications_promotion$workflow == "2117"] <- "Universe Cosmic Background"

pdf("PromotionSubmit.pdf", height=7, width=9)
hist_prom <- ggplot(classifications_promotion, aes(x=SubmitLevel, fill=UserLevel))
hist_prom + 
geom_bar() + 
theme (
	axis.text.x = element_text(size=10, angle=45)
	)
dev.off()

promotions_stats <- ddply(classifications_promotion, c("UserLevel"),summarise,
	Users = length(unique(login)),
	Neutron_Star_Mountain = sum(SubmitLevel=="Neutron Star Mountain"),
	Galactic_Supernova = sum(SubmitLevel=="Galactic Supernova"),
	Neutron_Star_Merger = sum(SubmitLevel=="Neutron Star Merger"),
	Black_Hole_Merger = sum(SubmitLevel=="Black Hole Merger"),
	Universe_Cosmic_Background = sum(SubmitLevel=="Universe Cosmic Background"))

classifications_promotion$Date <- as.POSIXct(as.character(classifications_promotion$Date),format="%m/%d/%y %H:%M") 

promotion_user <- ddply(classifications_promotion, c("login","UserLevel"),summarise,
	Date = min(Date),
	Classification = min(Classifications),
	Session = min(Session))




