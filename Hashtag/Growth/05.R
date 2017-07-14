#05

setwd("~/Documents/Academic/School/REU/hashtags/02")

#### Growth Modeling HT_c ####
m1 <- lme(cumul ~ I(week_num), data = HT_c, random = ~ 1 | tag, method = "ML")
m2 <- lme(cumul ~ I(week_num) + I(week_num^2), data = HT_c,
      random = ~ 1 | tag, method = "ML")
m3 <- lme(cumul ~ week_num + I(week_num^2) + I(week_num * users), data = HT_c,
      random = ~ 1 | tag, method = "ML")
m4 <- lme(cumul ~ I(log(week_num)), data = HT_c, random = ~ 1 | tag, method = "ML")
anova(m1, m2, m3, m4)

#### Growth Modeling HT_c100 ####
n1 <- lme(cumul ~ I(week_num), data = HT_c100, random = ~ 1 | tag, method = "ML")
n2 <- lme(cumul ~ I(week_num) + I(week_num^2), data = HT_c100,
      random = ~ 1 | tag, method = "ML")
n3 <- lme(cumul ~ week_num + I(week_num^2) + I(week_num * users), data = HT_c100,
      random = ~ 1 | tag, method = "ML")
n4 <- lme(cumul ~ I(log(week_num)), data = HT_c100, random = ~ 1 | tag,
      method = "ML")
anova(n1, n2, n3, n4)

#### Growth Modeling HT_v ####
q1 <- lme(cumul ~ I(day_num), data = HT_v, random = ~ 1 | tag, method = "ML")
q2 <- lme(cumul ~ I(day_num) + I(day_num^2), data = HT_v,
      random = ~ 1 | tag, method = "ML")
q3 <- lme(cumul ~ day_num + I(day_num^2) + I(day_num * users), data = HT_v,
      random = ~ 1 | tag, method = "ML")
q4 <- lme(cumul ~ I(log(day_num)), data = HT_v, random = ~ 1 | tag,
      method = "ML")
anova(q1, q2, q3, q4)

#### Growth Modeling HT_k ####
r1 <- lme(cumul ~ I(week_num), data = HT_k, random = ~ 1 | tag, method = "ML")
r2 <- lme(cumul ~ I(week_num) + I(week_num^2), data = HT_k,
      random = ~ 1 | tag, method = "ML")
r3 <- lme(cumul ~ week_num + I(week_num^2) + I(week_num * users), data = HT_k,
      random = ~ 1 | tag, method = "ML")
r4 <- lme(cumul ~ I(log(week_num)), data = HT_k, random = ~ 1 | tag,
      method = "ML")
anova(r1, r2, r3, r4)

#### Growth Modeling HT_k16 ####
s1 <- lme(cumul ~ I(week_num), data = HT_k16, random = ~ 1 | tag, method = "ML")
s2 <- lme(cumul ~ I(week_num) + I(week_num^2), data = HT_k16,
          random = ~ 1 | tag, method = "ML")
s3 <- lme(cumul ~ week_num + I(week_num^2) + I(week_num * users), data = HT_k16,
          random = ~ 1 | tag, method = "ML")
s4 <- lme(cumul ~ I(log(week_num)), data = HT_k16, random = ~ 1 | tag,
          method = "ML")
anova(s1, s2, s3, s4)

#### Growth Modeling HT_fk ####
fk_base <- lmer(cumul ~ week_num + users + p_user + intro_wk + (1 | tag),
      data = HT_fk, REML=F)
fk_type <- lmer(cumul ~ week_num + users + intro_wk + (1 | tag), data = HT_fk, REML = F)
fk_intro <- lmer(cumul ~ week_num + users + p_user + (1 | tag), data = HT_fk, REML = F)
fk_users <- lmer(cumul ~ week_num + p_user + intro_wk + (1 | tag), data = HT_fk, REML = F)
fk_wknum <- lmer(cumul ~ users + p_user + intro_wk + (1 | tag), data = HT_fk, REML = F)
anova(fk_base, fk_type)
anova(fk_base, fk_intro)
anova(fk_base, fk_users)
anova(fk_base, fk_wknum)
anova(fk_base, fk_type, fk_intro, fk_users, fk_wknum)

#### Growth Modeling HT_fk16 ####
fk16_base <- lmer(cumul ~ week_num + users + p_user + intro_wk + (1 | tag),
      data = HT_fk16, REML=F)
fk16_type <- lmer(cumul ~ week_num + users + intro_wk + (1 | tag), data = HT_fk16, REML = F)
fk16_intro <- lmer(cumul ~ week_num + users + p_user + (1 | tag), data = HT_fk16, REML = F)
fk16_users <- lmer(cumul ~ week_num + p_user + intro_wk + (1 | tag),
      data = HT_fk16, REML = F)
fk16_wknum <- lmer(cumul ~ users + p_user + intro_wk + (1 | tag), data = HT_fk16, REML = F)
anova(fk16_base, fk16_type)
anova(fk16_base, fk16_intro)
anova(fk16_base, fk16_users)
anova(fk16_base, fk16_wknum)

#### Growth Modeling HT_qk ####
qk_base <- lmer(cumul ~ week_num + users + p_user + intro_wk + (1 | tag),
      data = HT_qk, REML=F)
qk_type <- lmer(cumul ~ week_num + users + intro_wk + (1 | tag), data = HT_qk, REML = F)
qk_intro <- lmer(cumul ~ week_num + users + p_user + (1 | tag), data = HT_qk, REML = F)
qk_users <- lmer(cumul ~ week_num + p_user + intro_wk + (1 | tag), data = HT_qk, REML = F)
qk_wknum <- lmer(cumul ~ users + p_user + intro_wk + (1 | tag), data = HT_qk, REML = F)
anova(qk_base, qk_type)
anova(qk_base, qk_intro)
anova(qk_base, qk_users)
anova(qk_base, qk_wknum)

#### Growth Modeling HT_qk16 ####
qk16_base <- lmer(cumul ~ week_num + users + p_user + intro_wk + (1 | tag),
      data = HT_qk16, REML=F)
qk16_type <- lmer(cumul ~ week_num + users + intro_wk + (1 | tag), data = HT_qk16, REML = F)
qk16_intro <- lmer(cumul ~ week_num + users + p_user + (1 | tag), data = HT_qk16, REML = F)
qk16_users <- lmer(cumul ~ week_num + p_user + intro_wk + (1 | tag),
      data = HT_qk16, REML = F)
qk16_wknum <- lmer(cumul ~ users + p_user + intro_wk + (1 | tag), data = HT_qk16, REML = F)
anova(qk16_base, qk16_type)
anova(qk16_base, qk16_intro)
anova(qk16_base, qk16_users)
anova(qk16_base, qk16_wknum)

(ggplot(HT_h) + geom_path(aes(day, prior, group = tag)))
(ggplot(HT_h) + geom_path(aes(day, prior, group = tag)))
(ggplot(HT_h) + geom_path(aes(day, log(prior), group = tag)))
(ggplot(HT_h) + geom_path(aes(day, I(prior^2) + prior, group = tag)))

# GGPLOT for "mid-frequency line" - prior
ggplot(HT_h[tag == "midfrequencyline"]) + geom_path(aes(day, prior, group = "tag"))
# GGPLOT for "mid-frequency line" - cumulative
ggplot(HT_h[tag == "midfrequencyline"]) + geom_path(aes(day, prior, group = "tag"))

# GGPLOT for Tag Explansion Path
(ggplot(HT_c) + geom_path(aes(week_num, cumul, group = tag))
      + theme(axis.text.x = element_text(angle = 90)))

# GGPLOT for Tag Expansion Path with Tags over 16 weeks (simplified)
(ggplot(HT_a) + geom_path(aes(week_num, cumul, group = tag))
      + theme(axis.text.x = element_text(angle = 90)))

# GGPLOT for Regression of Week Number vs. Cumulative Count
(ggplot(HT_c, aes(week_num, cumul, color = tag))
      + stat_summary(fun.data = mean_se, geom = "point")
      + labs(y = "Cumulative Uses of Tags", x = "Week Number since the Introduction")
      + scale_color_manual(values=1:length(unique(HT_c$tag)))
      + theme_bw() + stat_summary(aes(y = fitted(m1)), fun.y = mean, geom = "line")
      + theme(legend.position = "none"))

# GGPLOT for Regression of Week Number vs. Cumulative Count over 100 (simplified)
(ggplot(HT_c100, aes(week_num, cumul, color = tag))
      + stat_summary(fun.data = mean_se, geom = "point")
      + labs(y = "Cumulative Uses of Tags", x = "Week Number since the Introduction")
      + scale_color_manual(values=1:length(unique(HT_c100$tag))) + theme_bw()
      + stat_summary(aes(y = fitted(n1)), fun.y = mean, geom = "line"))

(ggplot(HT_c100, aes(week_num, cumul))
      + stat_summary(fun.data = mean_se, geom = "pointrange")
      + labs(y = "Average Uses of Tags", x = "Week Number since the Introduction")
      + theme_bw() + stat_summary(aes(y = fitted(n1)), fun.y = mean, geom = "line"))

(ggplot(HT_c, aes(week_num, cumul, color = n_char))
      + stat_summary(fun.data = mean_se, geom = "linerange")
      + stat_summary(fun.data = mean_se, geom = "point")
      + labs(y = "Average Uses of Tags", x = "Week Number since the Introduction")
      + theme_bw() + stat_summary(aes(y = fitted(m1)), fun.y = mean, geom = "line"))

(ggplot(HT_c, aes(week_num, cumul, color = above_med_nchar))
      + stat_summary(fun.data = mean_se, geom = "pointrange")
      + labs(y = "Average Uses of Tags", x = "Week Number since the Introduction")
      + theme_bw() + stat_summary(aes(y = fitted(m1)), fun.y = mean, geom = "line"))

(ggplot(HT_c, aes(week_num, cumul, color = below_med_nchar))
      + stat_summary(fun.data = mean_se, geom = "pointrange")
      + labs(y = "Average Uses of Tags", x = "Week Number since the Introduction")
      + theme_bw() + stat_summary(aes(y = fitted(m1)), fun.y = mean, geom = "line"))

(ggplot(HT_c, aes(week_num, cumul, color = n_char == 9))
      + stat_summary(fun.data = mean_se, geom = "linerange")
      + stat_summary(fun.data = mean_se, geom = "point")
      + labs(y = "Average Uses of Tags", x = "Week Number since the Introduction")
      + theme_bw() + stat_summary(aes(y = fitted(m1)), fun.y = mean, geom = "line"))

(ggplot(HT_v, aes(day_num, cumul))
      + stat_summary(fun.data = mean_se, geom = "linerange")
      + stat_summary(fun.data = mean_se, geom = "point")
      + labs(y = "Average Uses of Tags", x = "Day since the Introduction")
      + theme_bw() + stat_summary(aes(y = fitted(q1)), fun.y = mean, geom = "line"))

(ggplot(HT_k, aes(week_num, cumul, color = p_user))
      + stat_summary(fun.data = mean_se, geom = "linerange")
      + stat_summary(fun.data = mean_se, geom = "point")
      + labs(y = "Average Uses of Tags", x = "Week Number since the Introduction")
      + theme_bw() + stat_summary(aes(y = fitted(r1)), fun.y = mean, geom = "line"))

(ggplot(HT_k16, aes(week_num, cumul, color = p_user))
      + stat_summary(fun.data = mean_se, geom = "linerange")
      + stat_summary(fun.data = mean_se, geom = "point")
      + labs(y = "Average Uses of Tags", x = "Week Number since the Introduction")
      + theme_bw() + stat_summary(aes(y = fitted(s1)), fun.y = mean, geom = "line"))

(ggplot(HT_fk, aes(week_num, cumul, color = p_user))
      + stat_summary(fun.data = mean_se, geom = "linerange")
      + stat_summary(fun.data = mean_se, geom = "point") + ggtitle("Whole")
      + labs(y = "Average Uses of Tags", x = "Week Number since the Introduction")
      + theme_bw() + stat_summary(aes(y = fitted(fk_base)), fun.y = mean, geom = "line"))

(ggplot(HT_fk16, aes(week_num, cumul, color = p_user))
      + stat_summary(fun.data = mean_se, geom = "linerange")
      + stat_summary(fun.data = mean_se, geom = "point") + ggtitle("Whole ~16 Weeks")
      + labs(y = "Average Uses of Tags", x = "Week Number since the Introduction")
      + theme_bw() + stat_summary(aes(y = fitted(fk16_base)), fun.y = mean, geom = "line"))

(ggplot(HT_qk, aes(week_num, cumul, color = p_user))
      + stat_summary(fun.data = mean_se, geom = "linerange")
      + stat_summary(fun.data = mean_se, geom = "point") + ggtitle("Launch")
      + labs(y = "Average Uses of Tags", x = "Week Number since the Introduction")
      + theme_bw() + stat_summary(aes(y = fitted(qk_base)), fun.y = mean, geom = "line"))

(ggplot(HT_qk16, aes(week_num, cumul, color = p_user))
      + stat_summary(fun.data = mean_se, geom = "linerange")
      + stat_summary(fun.data = mean_se, geom = "point") + ggtitle("Launch ~16 Weeks")
      + labs(y = "Average Uses of Tags", x = "Week Number since the Introduction")
      + theme_bw() + stat_summary(aes(y = fitted(qk16_base)), fun.y = mean, geom = "line"))
