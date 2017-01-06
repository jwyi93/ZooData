# Corey Jackson
# Syracuse University
# Zero-Inflated Poisson Regression (http://www.ats.ucla.edu/stat/r/dae/zipoisson.htm)



## Geordi Data

library(ggplot2)
library(pscl)
library(MASS)
library(boot)
library(psych)
# Upload data 

geordi <- read.csv("~/Dropbox/INSPIRE/Data/Message Experiment/Geordi/Useractivity_30Days.csv")
gravity.spy.experiment <- read.csv("~/Dropbox/INSPIRE/Data/Message Experiment/gravity-spy-experiment.csv")
GS_id_login <- read.csv("~/Dropbox/INSPIRE/Data/Message Experiment/GS_id_login.csv")
GS_geordi_11116 <- read.csv("~/Dropbox/INSPIRE/Data/System dumps/GS_geordi_11116.csv")
GS_annotations <- read.csv("~/Dropbox/INSPIRE/Data/System dumps/Classifications_GS/Gs_Class_20161213-no-duplicate-SessClass.csv")


# Transformations
GS_annotations$time1 <- as.POSIXct(GS_annotations$created_at, format="%Y-%m-%d %H:%M:%S")


# Group 2 only 30 days afer 12 October - from 10/12 to 11/12
geordi <- geordi[ which(geordi$Experiment_Duration_30days==2), ]

geordi <- merge(geordi,GS_id_login, 
	by.x=c("userID"), 
	by.y=("id"), 
	all.x=TRUE)

geordi <- merge(geordi,gravity.spy.experiment, 
	by=c("login"), 
	all.x=TRUE)

remove(GS_id_login,gravity.spy.experiment)

# Remove users who weren't in experiment
geordi <- geordi[ which(!is.na(geordi$cohort)), ]

user.info <- ddply(GS_annotations, c("user_name"), summarise,
	join = min(time1)
	)

geordi <- merge(geordi,user.info, 
	by.x=c("login"), 
	by.y=("user_name"), 
	all.x=TRUE)


geordi <- geordi[ which(!is.na(geordi$join)), ]
remove(user.info,GS_annotations)


### Combine activities
 [1] project-menu            talk-view               top-menu                open-field-guide        close-field-guide       view-profile-sidebar    view-discussion        
 [8] edit-post               update-comment          about-menu              search                  change-page             classify                metadata               
[15] register-link           register                favorite                login                   new-discussion          add-comment             view-profile-author    
[22] recent-comments-sidebar breadcrumb              footer-menu             reply-post              subject-image           hashtag-sidebar         discussion-time        
[29] logout                  view-subject-direct     search-back             like-post               delete-post             change-project-sidebar  unfavorite             
[36] subscribe               globe-menu              profile-menu            collect-menu            link-post               message-user            send-message           
[43] report-post             classificationStart

# interaction
project-menu 
top-menu
view-profile-sidebar
edit-post
about-menu
search
view-profile-author
recent-comments-sidebar
breadcrumb
footer-menu
hashtag-sidebar
subscribe
report-post
globe-menu
search-back
profile-menu 
collect-menu
delete-post
unfavorite

# consuming/learning
talk-view
open-field-guide
view-discussion 
favorite
subject-image
view-subject-direct

# creating content
new-discussion
add-comment
reply-post
like-post
link-post
message-user
send-message

# other (remove?)
close-field-guide
update-comment
change-page
classify
metadata
register-link
register
login
discussion-time
logout
classificationStart (?)
change-project-sidebar



######## Zero-inflated approach ########
  m1 <- zeroinfl(talkPageReferences ~ cohort + classifications,
  data = geordi)
summary(m1)

mnull <- update(m1, . ~ 1)

pchisq(2 * (logLik(m1) - logLik(mnull)), df = 6, lower.tail = FALSE)
