# Code for Survival Analysis with Chimp & See
# Corey Jackson, IBM Almaden Research ADLab, July 2016

# Download and load packages
install.package("survival")
install.package("KMsurv")

library(survival)
library(KMsurv)

# Functions used here
Surv()
coxph()
survfit()

# Resources used in this work
# https://rpubs.com/daspringate/survival
# https://www.openintro.org/download.php?file=survival_analysis_in_R&referrer=/stat/surv.php
# https://www.openintro.org/stat/surv.php

user_population_extended_session$start <- as.numeric(user_population_extended_session$start)
user_population_extended_session$stop <- as.numeric(user_population_extended_session$stop)

S <- Surv(
  time = user_population_extended_session$start, 
  time2 = user_population_extended_session$stop, 
  event = user_population_extended_session$survive)


# Building the survival model
model <- coxph(S ~ accuracy, 
               data = user_population_extended_session)

# Plot the baseline survival function
plot(survfit(model), 
     xscale = 365.25,
     xlab = "Years after diagnosis",
     ylab = "Proportion survived",
     main = "Baseline Hazard Curve")