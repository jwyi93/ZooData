# Analysis for Hashtag paper

#### Some requirements for the dataset
# Data only after the launch - Oct. 12 2016
# Remove tags that first appeared before launch

#libraries
library(lme4)



##### Summary Stats #####
sprintf("How many tags in dataset: %d Whole / %d Launch", length(unique(HT_f$tag)),
      length(unique(HT_q$tag)))
sprintf("Average time in dataset: %g Whole / %g Launch", mean(HT_fs$span), mean(HT_qs$span))
sprintf("How many users are using hashtags: %d Whole / %d Launch",
      length(unique(HT_f$user)), length(unique(HT_q$user)))
sprintf("How many users introduced tags: %d Whole / %d Launch", length(unique(HT_fi$user)),
      length(unique(HT_qi$user)))
# Days after project start are tags in system
## (create Figure. x = date, y = count of tags)
### Week
(ggplot(data = HT_fcw, aes(x = week, y = cumul)) + ggtitle("Whole") + ylab("Cumulative")
      + geom_bar(stat = "identity") + theme(axis.text.x = element_text(size = 8, angle=90)))
(ggplot(data = HT_qcw, aes(x = week, y = cumul)) + ggtitle("Launch") + ylab("Cumulative")
      + geom_bar(stat = "identity") + theme(axis.text.x = element_text(size = 8, angle=90)))
### Month
(ggplot(data = HT_fcm, aes(x = month, y = cumul)) + ggtitle("Whole") + ylab("Cumulative")
      + geom_bar(stat = "identity") + theme(axis.text.x = element_text(size = 8, angle=90)))
(ggplot(data = HT_qcm, aes(x = month, y = cumul)) + ggtitle("Launch") + ylab("Cumulative")
      + geom_bar(stat = "identity") + theme(axis.text.x = element_text(size = 8, angle=90)))
# Distribution of uses. How many times tags used 
## (e.g., 20% of tags used once, 40% used more than 10 occassions)
ggplot(HT_fnp, aes(uses, P)) + geom_col() + ggtitle("Whole") + ylab("Percentage")
ggplot(HT_qnp, aes(uses, P)) + geom_col() + ggtitle("Launch") + ylab("Percentage")
# Distribution of duration. (e.g., 20% tags around 1-5 days)
## You should create bins up to 30
(ggplot(data=HT_fd, aes(span)) + ggtitle("Whole") + ylab("Percentage")
      + geom_histogram(breaks = seq(min(0), max(HT_fd$span + 4), by = 5),
      col = "black", aes(y = (..count..)/sum(..count..))))
(ggplot(data=HT_qd, aes(span)) + ggtitle("Launch") + ylab("Percentage")
      + geom_histogram(breaks = seq(min(0), max(HT_qd$span + 4), by = 5),
      col = "black", aes(y = (..count..)/sum(..count..))))
ggplot(HT_fdp, aes(span, P)) + geom_bar() + ggtitle("Whole") + ylab("Percentage")
ggplot(HT_qnp, aes(uses, P)) + geom_col() + ggtitle("Launch") + ylab("Percentage")
# Distribution of users. (e.g., 20% tags were used by 1-5 users)
## You should create bins up to 20
(ggplot(data = HT_fu, aes(users)) + ggtitle("Whole") + ylab("Percentage")
      + geom_histogram(breaks = seq(min(0), max(HT_fu$users + 4), by = 5),
      col = "black", aes(y = (..count..)/sum(..count..))))
(ggplot(data = HT_qu, aes(users)) + ggtitle("Launch") + ylab("Percentage")
      + geom_histogram(breaks = seq(min(0), max(HT_qu$users + 4), by = 5),
      col = "black", aes(y = (..count..)/sum(..count..))))

##### Hypothesis Testing/Growth Models #####

# http://www.danmirman.org/gca
# https://www.khanacademy.org/science/biology/ecology/population-growth-and-regulation/a/exponential-logistic-growth


# H2 Tags introduced by authority sources (i.e., Power Users, moderators. You should create binary in dataset. In the example below I've named it type) have significantly greater growth curves.
m2.base <- lmer(use ~  time + users + type + (1 | tag), data=hashtags, REML=F)
m2.type <- lmer(use ~  time + users + (1 | tag), data=hashtags, REML=F)
anova(m2.base,m2.type)

# H1 Tags accompanied with use materials (e.g., Talk pages describing use) have significantly greater growth curves than those with no Talk pages.
m1.base <- lmer(use ~  time + users + materials + (1 | tag), data=hashtags, REML=F)
m1.materials <- lmer(use ~  time + users + (1 | tag), data=hashtags, REML=F)
anova(m1.materials,m1.base)


##### Predicting Hashtag Growth #####
# Based on popular hashtags can we predict which tags will go viral in future. 
