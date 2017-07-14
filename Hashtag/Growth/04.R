# Hashtag analysis. Dataset beginning 2016-04-04 14:01:14.92 ending 2017-04-29 21:20:35.16
# Measure everyday

#### Installing and setting up packages ####
install.packages("plyr", dependencies = TRUE)
library(plyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
install.packages("lme4")
library(lme4)


#### Reading .csv file ####
HT_f <- read.csv("~/Documents/Academic/School/REU/hashtags/02/hashtags_format.csv",
                 header=TRUE, sep=",")

#### Cleansing of Hashtag Format Dataframe ####
HT_f$tag <- as.character(HT_f$tag)
HT_f$day <- as.Date(substring(HT_f$created_at, 1, 10))
HT_f$month <- substring(HT_f$day, 1, 7)
HT_f$hour <- substring(HT_f$created_at, 12, 19)
HT_f$time <- paste(HT_f$day,HT_f$hour)
HT_f$X <- HT_f$document.id <- HT_f$hour <- HT_f$created_at <- NULL
HT_f$time <- as.POSIXct(as.character(HT_f$time), format="%Y-%m-%d %H:%M:%S")
HT_f$type <- as.character(HT_f$type)
HT_f$type[is.na(HT_f$type)] <- "Other"
HT_f$taggable_id[is.na(HT_f$taggable_id)] <- "0000000"

#### Dataset only after the Launch ####
HT_l <- data.table(HT_f[which(HT_f$day >= "2016/10/12"), ])
HT_l$array.index <- HT_l$array.index - min(HT_l$array.index) + 1

#### Dataset of tags introduced after the launch ####
HT_q <- ddply(HT_f, ~ tag, summarize, launch = min(day) >= "2016/10/12")
HT_q <- HT_q[which(HT_q$launch == 1), ]
HT_q <- merge(HT_f, HT_q, by = c("tag"))
HT_q <- HT_q[with(HT_q, order(array.index)), ]

#### Creating daily hashtag explosions ####
HT_e1 <- data.table(count(HT_f, vars = c("day", "tag")))
HT_e1[, prior := cumsum(freq), by = list(tag)]
HT_e1$prior <- HT_e1$prior - HT_e1$freq
HT_e2 <- ddply(HT_f, ~ day + tag, summarize, users = length(unique(user)))
HT_e <- merge(HT_e1, HT_e2, by = c("day", "tag"))
remove(HT_e1, HT_e2)

#### Creating weekly hashtag explosions ####
HT_d1 <- data.table(count(HT_f, vars = c(week = "format(HT_f$day, \"%Y-%U\")", "tag")))
HT_d1[, prior := cumsum(freq), by = list(tag)]
HT_d1$prior <- HT_d1$prior - HT_d1$freq
HT_d2 <- ddply(HT_f, ~ format(HT_f$day, "%Y-%U") + tag, summarize,
      users = length(unique(user)))
names(HT_d2) = c("week", "tag", "users")
HT_d <- merge(HT_d1, HT_d2, by = c("week", "tag"))
remove(HT_d1, HT_d2)

#### Creating normalized weekly hashtags that have occurred over 16 weeks ####
HT_a <- data.frame(tag = as.character(rownames(table(HT_d$tag) >= 16)),
      over16 = table(HT_d$tag) >= 16)
HT_a <- merge(HT_d, HT_a[HT_a$over16 != "FALSE",], by = c("tag"))
HT_a$week_of <- as.Date(paste(HT_a$week, 1, sep = "-"), "%Y-%U-%u")
first_tags <- ddply(HT_a, c("tag"), summarize,
      min = min(week_of), max = min(week_of) + 120)
HT_a <- merge(HT_a, first_tags, by = c("tag"))
HT_a$use <- ifelse(HT_a$week_of >=  HT_a$min & HT_a$week_of <= HT_a$max, "1", "0")
HT_a <- HT_a[which(HT_a$use == '1'),]
HT_a[, cumul := cumsum(freq), by = list(tag)]
HT_a$week_num <- as.integer(HT_a$week_of)
HT_a$week_num <- (HT_a$week_num - min(HT_a$week_num) + 7)/7
HT_a$week_num <- ave(HT_a$week_num, HT_a$tag, FUN = function(x) c(1, diff(x)))
HT_a[, week_num := cumsum(week_num), by = list(tag)]
HT_a <- HT_a[HT_a$week_num < 17, ]
HT_a$week <- HT_a$over16 <- HT_a$min <- HT_a$max <- HT_a$use <- NULL
remove(first_tags)

#### Creating normalized weekly hashtags ####
HT_c <- HT_d
HT_c$week_of <- as.Date(paste(HT_c$week, 1, sep = "-"), "%Y-%U-%u")
first_tags <- ddply(HT_c, c("tag"), summarize,
      min = min(week_of), max = min(week_of) + 120)
HT_c <- merge(HT_c, first_tags, by = c("tag"))
HT_c$use <- ifelse(HT_c$week_of >=  HT_c$min & HT_c$week_of <= HT_c$max, "1", "0")
HT_c <- HT_c[which(HT_c$use == '1'),]
HT_c[, cumul := cumsum(freq), by = list(tag)]
HT_c$week_num <- as.integer(HT_c$week_of)
HT_c$week_num <- (HT_c$week_num - min(HT_c$week_num) + 7)/7
HT_c$week_num <- ave(HT_c$week_num, HT_c$tag, FUN = function(x) c(1, diff(x)))
HT_c[, week_num := cumsum(week_num), by = list(tag)]
HT_c <- HT_c[HT_c$week_num < 17, ]
HT_c$tag <- as.character(HT_c$tag)
HT_c$week <- HT_c$min <- HT_c$max <- HT_c$use <- NULL
HT_c$n_char <- as.character(nchar(HT_c$tag))
HT_c$above_med_nchar <- (nchar(HT_c$tag) > median(nchar(HT_c$tag)))
HT_c$below_med_nchar <- (nchar(HT_c$tag) < median(nchar(HT_c$tag)))
remove(first_tags)

#### Obvervations over 100 ####
HT_c100 <- data.frame(tag = unique(HT_c$tag[HT_c$cumul > 100]))
HT_c100 <- merge(HT_c, HT_c100, by = "tag")

#### Creating Frequency of Tags by Months ####
HT_e$month <- substring(HT_e$day, 1, 7)
HT_g <- count(ddply(HT_f, c("tag"), summarize, month = month))
HT_g$freq <- NULL

#### Previous chart of appearance more than 2 months ####
HT_h = data.frame(tag = unique(HT_g$tag), app = table(HT_g$tag) >= 4,
                  row.names = NULL)
HT_h = HT_h[HT_h$app != FALSE,]
HT_h <- merge(HT_g, HT_h, by = c("tag"), key = "month")
HT_h <- merge(HT_e, HT_h, by = c("tag", "month"))
HT_h$app <- HT_h$month <- NULL
setorder(HT_h, day, tag)
remove(HT_g)

HT_cavg <- ddply(HT_c, c("week_num"), summarize, avg_cumul = mean(cumul),
      min_cumul = min(cumul), max_cumul = max(cumul), med_cumul = median(cumul))

#### Power user ####
HT_o1 <- ddply(HT_f, ~ day + tag + user, summarize, freq = table(tag))
HT_o2 <- data.frame(table(user = HT_f$user))
HT_o2$p_user <- HT_o2$Freq > quantile(HT_o2$Freq)[4]
HT_o2$Freq <- NULL
HT_o <- merge(HT_o1, HT_o2, by = c("user"))
remove(HT_o1, HT_o2)

#### Weekly observations with power user ####
HT_fk1 <- ddply(HT_o, ~ day + tag + p_user, summarize, users = length(user))
HT_fk1$week <- format(HT_fk1$day, "%Y-%U")
HT_fk1$week_of <- as.Date(paste(HT_fk1$week, 1, sep = "-"), "%Y-%U-%u")
HT_fk1$week <- NULL
HT_fk2 <- as.data.table(ddply(HT_fk1, ~ format(HT_fk1$day, "%Y-%U"),
      summarize, unique(tag)))
names(HT_fk2) <- c("week", "tag")
HT_fk2$week_of <- as.Date(paste(HT_fk2$week, 1, sep = "-"), "%Y-%U-%u")
HT_fk2$week_num <- as.integer(HT_fk2$week_of)
HT_fk2$week_num <- (HT_fk2$week_num - min(HT_fk2$week_num) + 7)/7
HT_fk2$week_num <- ave(HT_fk2$week_num, HT_fk2$tag, FUN = function(x) c(1, diff(x)))
HT_fk2[, week_num := cumsum(week_num), by = list(tag)]
HT_fk3 <- as.data.table(count(HT_o, vars = c(week = "format(HT_o$day, \"%Y-%U\")",
      "tag", "p_user")))
HT_fk3$week_of <- as.Date(paste(HT_fk3$week, 1, sep = "-"), "%Y-%U-%u")
HT_fk3[, cumul := cumsum(freq), by = list(tag, p_user)]
HT_fk3$week <- NULL
HT_fk4 <- ddply(HT_o, ~ tag + p_user + format(day, "%Y-%U"),
      summarize, users = length(user))
names(HT_fk4) <- c("tag", "p_user", "week", "users")
HT_fk <- merge(HT_fk2, HT_fk3, by = c("tag", "week_of"))
HT_fk <- merge(HT_fk, HT_fk4, by = c("tag", "week", "p_user"))
intro_wk <- ddply(HT_fk, ~ tag, summarize, intro_wk = min(week_of))
HT_fk <- merge(HT_fk, intro_wk, by = c("tag"))
remove(HT_fk1, HT_fk2, HT_fk3, HT_fk4, intro_wk)
HT_fk16 <- HT_fk[which(HT_fk$week_num <= 16)]

#### First week observation ####
HT_v <- ddply(HT_f, ~ tag + day, summarize, users = length(unique(user)),
              freq = as.integer(table(tag)))
first_tags <- ddply(HT_v, c("tag"), summarize, min = min(day) - 1, max = min(day) + 7)
HT_v <- as.data.table(merge(HT_v, first_tags, by = c("tag")))
HT_v$uses <- ifelse(HT_v$day > HT_v$min & HT_v$day < HT_v$max, 1, 0)
HT_v <- HT_v[HT_v$uses != 0, ]
HT_v$min <- HT_v$max <- HT_v$uses <- NULL
HT_v$day_num <- as.integer(HT_v$day)
HT_v$day_num <- ave(HT_v$day_num, HT_v$tag, FUN = function(x) c(1, diff(x)))
HT_v[, day_num := cumsum(day_num), by = list(tag)]
HT_v$month <- substring(HT_v$day, 1, 7)
HT_v[, cumul := cumsum(freq), by = list(tag)]
over_two_days <- as.data.frame(table(tag = HT_v$tag[HT_v$day_num != 1]))
over_two_days$tag <- as.character(over_two_days$tag)
over_two_days$Freq <- NULL
HT_v <- merge(HT_v, over_two_days, by = c("tag"))
remove(first_tags, over_two_days)

#### Introduced by a power user ####
HT_k1 <- ddply(HT_f, ~ tag, summarize, time = min(time))
HT_k1 <- merge(HT_k1, HT_f, by = c("tag", "time"))
HT_k2 <- data.frame(table(user = HT_f$user))
HT_k2$p_user <- HT_k2$Freq > quantile(HT_k2$Freq)[4]
HT_k3 <- ddply(HT_f, c("tag", week = "format(day, \"%Y-%U\")"), summarize,
      users = length(user))
HT_k <- merge(HT_k1, HT_k2, by = c("user"))
HT_k$array.index <- HT_k$id <- HT_k$user_id <- HT_k$time <- HT_k$type <-
      HT_k$comment_id <- HT_k$taggable_id <- HT_k$month <- HT_k$Freq <- HT_k$user <-
      HT_k$day <- NULL
HT_k <- merge(HT_k, HT_f, by = c("tag"))
HT_k$array.index <- HT_k$id <- HT_k$user_id <- HT_k$time <- HT_k$type <-
      HT_k$comment_id <- HT_k$taggable_id <- HT_k$month <- HT_k$Freq <- NULL
HT_k$week <- format(HT_k$day, "%Y-%U")
HT_k <- data.table(ddply(HT_k, ~ tag + week + p_user, summarize, freq = length(tag)))
HT_k$week_of <- as.Date(paste(HT_k$week, 1, sep = "-"), "%Y-%U-%u")
HT_k[, cumul := cumsum(freq), by = list(tag)]
HT_k$week_num <- as.integer(HT_k$week_of)
HT_k$week_num <- (HT_k$week_num - min(HT_k$week_num) + 7)/7
HT_k$week_num <- ave(HT_k$week_num, HT_k$tag, FUN = function(x) c(1, diff(x)))
HT_k[, week_num := cumsum(week_num), by = list(tag)]
HT_k <- merge(HT_k, HT_k3, by = c("tag", "week")); HT_k$week <- NULL
remove(HT_k1, HT_k2, HT_k3)
HT_k16 <- HT_k[which(HT_k$week_num <= 16)]

#### Hashtags introduced after the launch ####
HT_qk1 <- merge(HT_q, ddply(HT_q, ~ tag, summarize, time = min(time)),
      by = c("tag", "time"))
HT_qk2 <- data.frame(table(user = HT_q$user))
HT_qk2$p_user <- HT_qk2$Freq > quantile(HT_qk2$Freq)[4]
HT_qk3 <- ddply(HT_q, c("tag", week = "format(day, \"%Y-%U\")"), summarize,
      users = length(user))
HT_qk <- merge(HT_qk1, HT_qk2, by = c("user"))
HT_qk$array.index <- HT_qk$id <- HT_qk$user_id <- HT_qk$time <- HT_qk$type <-
      HT_qk$comment_id <- HT_qk$taggable_id <- HT_qk$month <- HT_qk$Freq <- HT_qk$user <-
      HT_qk$day <- NULL
HT_qk <- merge(HT_qk, HT_q, by = c("tag"))
HT_qk$array.index <- HT_qk$id <- HT_qk$user_id <- HT_qk$time <- HT_qk$type <-
  HT_qk$comment_id <- HT_qk$taggable_id <- HT_qk$month <- HT_qk$Freq <- NULL
HT_qk$week <- format(HT_qk$day, "%Y-%U")
HT_qk <- data.table(ddply(HT_qk, ~ tag + week + p_user, summarize, freq = length(tag)))
HT_qk$week_of <- as.Date(paste(HT_qk$week, 1, sep = "-"), "%Y-%U-%u")
HT_qk[, cumul := cumsum(freq), by = list(tag)]
HT_qk$week_num <- as.integer(HT_qk$week_of)
HT_qk$week_num <- (HT_qk$week_num - min(HT_qk$week_num) + 7)/7
HT_qk$week_num <- ave(HT_qk$week_num, HT_qk$tag, FUN = function(x) c(1, diff(x)))
HT_qk[, week_num := cumsum(week_num), by = list(tag)]
HT_qk <- merge(HT_qk, HT_qk3, by = c("tag", "week"))
intro_wk <- ddply(HT_qk, ~ tag, summarize, intro_wk = min(week_of))
HT_qk <- merge(HT_qk, intro_wk, by = c("tag"))
HT_qk16 <- HT_qk[which(HT_qk$week_num <= 16)]
remove(HT_qk1, HT_qk2, HT_qk3, intro_wk)
