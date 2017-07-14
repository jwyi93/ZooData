#### How many users introduced tags ####
HT_fi <- merge(HT_f, ddply(HT_f, ~ tag, summarize, time = min(time)),
               by = c("tag", "time"))
HT_qi <- merge(HT_q, ddply(HT_q, ~ tag, summarize, time = min(time)),
               by =  c("tag", "time"))

#### Days after project start are tags in system (Whole / Launch) ####
HT_fc <- ddply(HT_f, c("tag"), summarize, week = format(day, "%Y-%U"), month = month)
HT_fcw <- data.frame(table(week = HT_fc$week)); HT_fcw$cumul <- cumsum(HT_fcw$Freq)
HT_fcm <- data.frame(table(month = HT_fc$month)); HT_fcm$cumul <- cumsum(HT_fcm$Freq)
HT_qc <- ddply(HT_q, c("tag"), summarize, week = format(day, "%Y-%U"), month = month)
HT_qcw <- data.frame(table(week = HT_qc$week)); HT_qcw$cumul <- cumsum(HT_qcw$Freq)
HT_qcm <- data.frame(table(month = HT_qc$month)); HT_qcm$cumul <- cumsum(HT_qcm$Freq)

#### Span of Hashtags ####
HT_fs <- ddply(HT_f, ~ tag, summarize, span = max(day) - min(day) + 1)
HT_qs <- ddply(HT_q, ~ tag, summarize, span = max(day) - min(day) + 1)

#### Number ####
HT_fn <- data.frame(table(tag = HT_f$tag)); HT_fn <- data.frame(table(uses = HT_fn$Freq))
HT_fn$uses <- as.integer(HT_fn$uses)
HT_fn$uses <- ifelse(HT_fn$uses > 10, 11, HT_fn$uses)
HT_fnp <- ddply(HT_fn, ~ uses, summarize, N = sum(Freq)); HT_fnp$P <- HT_fnp$N/sum(HT_fnp$N)
HT_qn <- data.frame(table(tag = HT_q$tag)); HT_qn <- data.frame(table(uses = HT_qn$Freq))
HT_qn$uses <- as.integer(HT_qn$uses)
HT_qn$uses <- ifelse(HT_qn$uses > 10, 11, HT_qn$uses)
HT_qnp <- ddply(HT_qn, ~ uses, summarize, N = sum(Freq)); HT_qnp$P <- HT_qnp$N/sum(HT_qnp$N)

#### Distribution of duration ####
HT_fd <- ddply(HT_f, ~ tag, summarize, span = max(day) - min(day) + 1)
HT_fd$span <- ifelse(HT_fd$span > 30, 31, HT_fd$span);
HT_fd <- data.frame(table(span = HT_fd$span)); HT_fd$span <- as.integer(HT_fd$span)
HT_fdp <- ddply(HT_fd, c(span = "as.integer(span)"), summarize, N = sum(Freq))
HT_fdp$P <- HT_fdp$N/sum(HT_fdp$N)
HT_qd <- ddply(HT_q, ~ tag, summarize, span = max(day) - min(day) + 1)
HT_qd$span <- ifelse(HT_qd$span > 30, 31, HT_qd$span);
HT_qd <- data.frame(table(span = HT_qd$span)); HT_qd$span <- as.integer(HT_qd$span)
HT_qdp <- ddply(HT_qd, c(span = "as.integer(span)"), summarize, N = sum(Freq))
HT_qdp$P <- HT_qdp$N/sum(HT_qdp$N)

#### Distribution of users ####
HT_fu <- ddply(HT_f, ~ tag, summarize, users = length(user))
HT_fu$users <- ifelse(HT_fu$users > 20, 21, HT_fu$users)
HT_fu <- data.frame(table(users = HT_fu$users)); HT_fu$users <- as.integer(HT_fd$users)
HT_fup <- ddply(HT_fu, c(users = "as.integer(users)"), summarize, N = sum(Freq))
HT_fup$P <- HT_fup$N/sum(HT_fup$N)
HT_qu <- ddply(HT_q, ~ tag, summarize, users = length(user))
HT_qu$users <- ifelse(HT_qu$users > 20, 21, HT_qu$users)
HT_qu <- data.frame(table(users = HT_qu$users)); HT_qu$users <- as.integer(HT_qu$users)
HT_qup <- ddply(HT_qu, c(users = "as.integer(users)"), summarize, N = sum(Freq))
HT_qup$P <- HT_qup$N/sum(HT_qup$N)