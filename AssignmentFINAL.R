### Library management
# Load necessary library
packages = c("survival","caret", "dplyr", "xtable", "ggfortify", "ggplot2")

# Function to check if package is installed
is_installed = function(pkg) {
  pkg %in% installed.packages()
}

# Install missing packages
install_missing_packages = function(pkg) {
  if (!is_installed(pkg)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Loop through packages and install missing ones
invisible(sapply(packages, install_missing_packages))

# Function to load package
load_package = function(pkg) {
  library(pkg, character.only = TRUE)
}

# Loop through packages and load them
invisible(sapply(packages, load_package))

#Print the package name and its version using a for loop
for (pkg in packages) {
  print(pkg)
  print(packageVersion(pkg))
}

### Data loading
#Read data into df
data = read.csv("/Users/Martijn/Library/Mobile Documents/com~apple~CloudDocs/School/Year 5/Q1/Survival Analysis for Data Scientists [2AMS11]/Project/DirtSlurper3000-2.csv", sep=';')

#Drop first row, because of faulty data
data = data[-1,]



### Preprocessing
#set Yes/No to 1/0
data$Sent.for.repair = ifelse(data$Sent.for.repair == "YES", 1, 0)

#set all status damage/ok to 1/0
data$Battery.status = ifelse(data$Battery.status == "Damage", 1,0)
data$Impact.status = ifelse(data$Impact.status == "Damage", 1,0)
data$IR.status = ifelse(data$IR.status == "Damage", 1,0)

#set registration date to date type
data$Registration.date = as.Date(data$Registration.date, format = "%d%b%Y")

#set failure date to date type
data$Failure.date = as.Date(data$Failure.date, format = "%d%b%Y")

#Add censoring date
data$Censor.date = rep(as.Date("31Dec2019", format= "%d%b%Y"),nrow(data))

#Add survival time column
fail = (data$Failure.date-data$Registration.date)
fail[is.na(fail)] = Inf
censor = (data$Censor.date-data$Registration.date)
data$Survival.time = pmin(fail,censor)
data$Survival.time.hours = as.numeric(data$Survival.time) * 24

#Add intensity column
data$Intensity.minutes.per.day = ifelse(data$Total.usage.time == 0, 0,
                           (data$Total.usage.time) / as.numeric(data$Survival.time.hours) * 60)


#Show unique values of Carpet.score where Impact.status is 1. Used in Cox regression part
unique(data[data$Impact.status == 1,]$Carpet.score)

#Show unique values of Carpet.score where IR.status is 1. Used in Cox regression part
unique(data[data$IR.status == 1,]$Carpet.score)

#Show unique values of Carpet.score where Battery.status is 1. Used in Cox regression part
unique(data[data$Battery.status == 1,]$Carpet.score)

#OneHotEncode Carpet.score column
data$Carpet.score = as.factor(data$Carpet.score) #Convert the Carpet.score variable to a factor 
dummy = dummyVars(~ Carpet.score, data = data) #One-hot encode using caret's dummyVars function
encoded_data = predict(dummy, newdata = data) %>% as.data.frame() #Create a dataframe with the encoded columns
data = data %>% select(-Carpet.score) %>% bind_cols(encoded_data) #Combine the original dataset (without Carpet.score) with the one-hot encoded columns



###Survival analysis - Impact sensor
#Create survival object
data_survival = Surv(time = data$Survival.time, event = data$Impact.status)

#Construct Kaplan-Meier estimator
results.km <- survfit(data_survival ~ 1, conf.type="plain", conf.int=0.95, data=data)
plot(results.km, conf.int = TRUE, ylim=c(0,1), xlab="Time (days)", ylab="Survival probability",
     cex.lab=1.5, cex.axis=1.3, lwd=1.5) # Plot the Kaplan-Meier estimator
title("Kaplan-Meier estimator for impact sensor with 95% confidence band", cex.main=1.8)
legend("bottomleft", legend=c("KM estimate"), col=c("black"), lty=1, cex=1.5, lwd=1.5)

#Construct Nelson-Aalen estimator
results.fh = survfit(data_survival ~ 1, stype=2) # First construct Fleming-Harrington estimator
plot(results.fh, fun="cumhaz", xlab = "Time (Days)", ylab = "Cumlative Hazard",
     cex.lab=1.5, cex.axis=1.3, lwd=1.5) # Plot Nelson-Aalen estimator
title("Nelson-Aalen estimator for impact sensor with 95% confidence bands", cex.main=1.8)
legend("topleft", legend=c("NA estimate"), lty=1, col=c("black"), cex=1.5, lwd=1.5)

#Cox proportional hazards model
res.cox <- coxph(data_survival ~ Intensity.minutes.per.day + Pets + Carpet.score.2 +
                   Carpet.score.3 + Carpet.score.4 + Carpet.score.5, data=data) #Carpet.score.6 to 9 is not used, because not in data for Impact.sensor==1
cox.zph(res.cox) #Check the proportional hazards assumption
print(xtable(as.data.frame(cox.zph(res.cox)$table) ), type="latex") #Output above table as latex
summary(res.cox) #Print the summary of the Cox proportional hazards model #NOTE: As the proportional hazards assumption is violated, these results are not reliable

#Plot survival function with pets and with no pets to sanity check violation of proportional hazards assumption on Pets
results.km.pets <- survfit(data_survival ~ Pets, data=data)
plot(results.km.pets, conf.int = TRUE, ylim=c(0,1), xlab="Time (days)", 
     ylab="Survival probability", col = c("black", "blue"), cex.lab=1.5, 
     cex.axis=1.3, lwd=1.5) # Plot the Kaplan-Meier estimator
title("Kaplan-Meier estimator for Impact sensor with and without pets with 95% confidence bands", cex.main=1.5)
legend("bottomleft", legend=c("No pets", "Pets"), col=c("black", "blue"), 
       lty=1, cex=1.5, lwd=1.5)

#Plot Nelson-Aalen estimator with pets and with no pets to sanity check violation of proportional hazards assumption on Pets
results.fh = survfit(data_survival ~ Pets, stype=2, data=data) # First construct Fleming-Harrington estimator
plot(results.fh, fun="cumhaz", conf.int=TRUE, xlab = "Time", ylab = "Cumlative Hazard", 
     col = c("black", "blue"), cex.lab=1.5, cex.axis=1.3, lwd=1.5) # Plot Nelson-Aalen estimator
title("Nelson-Aalen estimator for Impact sensor with and without pets with 95% confidence bands", cex.main=1.5)
legend("topleft", legend=c("No pets", "Pets"), col=c("black", "blue"), lty=1,
       cex=1.5, lwd=1.5)


# Stratified Cox proportional hazards model on Pets
#As the proportional hazards assumption is violated, we will stratify the Cox model
res.cox.strat <- coxph(data_survival ~ Intensity.minutes.per.day + strata(Pets) + Carpet.score.2 +
                         Carpet.score.3 + Carpet.score.4 + Carpet.score.5, data=data) #Carpet.score.6 to 9 is not used, because not in data for Impact.sensor==1
cox.zph(res.cox.strat) #Check the proportional hazards assumption
print(xtable(as.data.frame(cox.zph(res.cox.strat)$table) ), type="latex") #Output above table as latex
summary(res.cox.strat) #Print the summary of the stratified Cox proportional hazards model
print(xtable(as.data.frame(summary(res.cox.strat)$coefficients) ), type="latex") #Output the coefficients table as latex
print(xtable(as.data.frame(summary(res.cox.strat)$conf.int) ), type="latex") #Output the coefficients CI's table as latex

#######L10 analysis
survimpact = Surv(data$Survival.time, data$Impact.status) # Create survival object
fitimpact = survfit(survimpact ~ 1, data=data) # Construct Kaplan-Meier estimator

impact_sum = summary(fitimpact) # Summarize the Kaplan-Meier estimator
L10_impact = impact_sum$time[which.min(abs(impact_sum$surv - 0.90))] # Find the time at which the survival probability is 0.90
upper_impact = impact_sum$time[which.min(abs(impact_sum$upper - 0.90))] # Find the time at which the upper confidence interval is 0.90

if (L10_impact < upper_impact) {
  print("L10_impact is less than upper_impact")
  print(paste("Value of L10_impact is:", L10_impact))
  print(paste("Value of upper_impact is:", upper_impact))
} else {
  print("L10_impact is equal to upper_impact")
  print(paste("Value of L10_impact is:", L10_impact))
  print(paste("Value of upper_impact is:", upper_impact))
}


#Stratify on pets
results.km.pets <- survfit(data_survival ~ Pets, data=data)
impact_pets_sum = summary(results.km.pets) # Summarize the Kaplan-Meier estimator

#Split dfs on pets
strata_levels = levels(impact_pets_sum$strata)

#Pets = 0
#Find the rows corresponding to the current stratum
stratum_indices <- impact_pets_sum$strata == strata_levels[1]

#Extract the time, survival probability, and confidence intervals for the stratum
time_stratum = impact_pets_sum$time[stratum_indices] 
surv_stratum = impact_pets_sum$surv[stratum_indices]
lower_stratum = impact_pets_sum$lower[stratum_indices]
upper_stratum = impact_pets_sum$upper[stratum_indices]

#Create a dataframe
impact_summary_nopets = data.frame(
  Time = time_stratum,
  Survival = surv_stratum,
  Lower_CI = lower_stratum,
  Upper_CI = upper_stratum
) 

#Pets = 1
#Find the rows corresponding to the current stratum
stratum_indices <- impact_pets_sum$strata == strata_levels[2]

#Extract the time, survival probability, and confidence intervals for the stratum
time_stratum = impact_pets_sum$time[stratum_indices]
surv_stratum = impact_pets_sum$surv[stratum_indices]
lower_stratum = impact_pets_sum$lower[stratum_indices]
upper_stratum = impact_pets_sum$upper[stratum_indices]

#Create a data frame
impact_summary_yespets = data.frame(
  Time = time_stratum,
  Survival = surv_stratum,
  Lower_CI = lower_stratum,
  Upper_CI = upper_stratum
) 

#L10 no pets
L10_impact_nopets = impact_summary_nopets$Time[which.min(abs(impact_summary_nopets$Survival - 0.90))]
upper_impact_nopets = impact_summary_nopets$Time[which.min(abs(impact_summary_nopets$Upper_CI - 0.90))]

if (L10_impact_nopets < upper_impact_nopets) {
  print("L10_impact_nopets is less than upper_impact_nopets")
  print(paste("Value of L10_impact_nopets is:", L10_impact_nopets))
  print(paste("Value of upper_impact_nopets is:", upper_impact_nopets))
} else {
  print("L10_impact_nopets is equal to upper_impact_nopets")
  print(paste("Value of L10_impact_nopets is:", L10_impact_nopets))
  print(paste("Value of upper_impact_nopets is:", upper_impact_nopets))
}

#L10 yes pets
L10_impact_yespets = impact_summary_yespets$Time[which.min(abs(impact_summary_yespets$Survival - 0.90))]
upper_impact_yespets = impact_summary_yespets$Time[which.min(abs(impact_summary_yespets$Upper_CI - 0.90))]

if (L10_impact_yespets < upper_impact_yespets) {
  print("L10_impact_yespets is less than upper_impact_yespets")
  print(paste("Value of L10_impact_yespets is:", L10_impact_yespets))
  print(paste("Value of upper_impact_yespets is:", upper_impact_yespets))
} else {
  print("L10_impact_yespets is equal to upper_impact_yespets")
  print(paste("Value of L10_impact_yespets is:", L10_impact_yespets))
  print(paste("Value of upper_impact_yespets is:", upper_impact_yespets))
}



###Survival analysis - IR sensor
#Create survival object
data_survival = Surv(time = data$Survival.time, event = data$IR.status)

#Construct Kaplan-Meier estimator
results.km <- survfit(data_survival ~ 1, conf.type="plain", conf.int=0.95, data=data)
plot(results.km, conf.int = TRUE, ylim=c(0,1), xlab="Time (days)", 
     ylab="Survival probability", cex.lab=1.5, cex.axis=1.3, lwd=1.5) # Plot the Kaplan-Meier estimator
title("Kaplan-Meier estimator for IR sensor with 95% confidence band", cex.main=1.5)
legend("bottomleft", legend=c("KM estimate"), col=c("black"), lty=1, cex=1.5, lwd=1.5)

#Construct Nelson-Aalen estimator
results.fh = survfit(data_survival ~ 1, stype=2) # First construct Fleming-Harrington estimator
plot(results.fh, fun="cumhaz", xlab = "Time (Days)", ylab = "Cumlative Hazard",
     cex.lab=1.5, cex.axis=1.3, lwd=1.5) # Plot Nelson-Aalen estimator
title("Nelson-Aalen estimator for IR sensor with 95% confidence bands", cex.main=1.5)
legend("topleft", legend=c("NA estimate"), lty=1, col=c("black"), cex=1.5, lwd=1.5)

#Cox proportional hazards model
res.cox <- coxph(data_survival ~ Intensity.minutes.per.day, data=data)
cox.zph(res.cox) #Check the proportional hazards assumption
print(xtable(as.data.frame(cox.zph(res.cox)$table) ), type="latex") #Output above table as latex
summary(res.cox) #Print the summary of the Cox proportional hazards model
print(xtable(as.data.frame(summary(res.cox)$coefficients) ), type="latex") #Output the coefficients table as latex
print(xtable(as.data.frame(summary(res.cox)$conf.int) ), type="latex") #Output the coefficients (CI's) table as latex

####L10 analysis
survir = Surv(data$Survival.time, data$IR.status) # Create survival object
fitir = survfit(survir ~ 1, data=data) # Construct Kaplan-Meier estimator

ir_sum = summary(fitir) # Summarize the Kaplan-Meier estimator
L10_ir = ir_sum$time[which.min(abs(ir_sum$surv - 0.90))] # Find the time at which the survival probability is 0.90
upper_ir = ir_sum$time[which.min(abs(ir_sum$upper - 0.90))] # Find the time at which the upper confidence interval is 0.90

if (L10_ir < upper_ir) {
  print("L10_ir is less than upper_ir")
  print(paste("Value of L10_ir is:", L10_ir))
  print(paste("Value of upper_ir is:", upper_ir))
} else {
  print("L10_ir is equal to upper_ir")
  print(paste("Value of L10_ir is:", L10_ir))
  print(paste("Value of upper_ir is:", upper_ir))
}


###Survival analysis - Battery
#Create survival object
data_survival = Surv(time = data$Survival.time, event = data$Battery.status)

#Construct Kaplan-Meier estimator
results.km <- survfit(data_survival ~ 1, conf.type="plain", conf.int=0.95, data=data)
plot(results.km, conf.int = TRUE, ylim=c(0,1), xlab="Time (days)", 
     ylab="Survival probability", cex.lab=1.5, cex.axis=1.3, lwd=1.5) # Plot the Kaplan-Meier estimator
title("Kaplan-Meier estimator for battery with with 95% confidence band", cex.main=1.5)
legend("bottomleft", legend=c("KM estimate"), col=c("black"), lty=1, cex=1.5, lwd=1.5)

#Construct Nelson-Aalen estimator
results.fh = survfit(data_survival ~ 1, stype=2) # First construct Fleming-Harrington estimator
plot(results.fh, fun="cumhaz", xlab = "Time (Days)", 
     ylab = "Cumlative Hazard", cex.lab=1.5, cex.axis=1.3, lwd=1.5) # Plot Nelson-Aalen estimator
title("Nelson-Aalen estimator for battery with 95% confidence bands", cex.main=1.5)
legend("topleft", legend=c("NA estimate"), lty=1, col=c("black"), cex=1.5, lwd = 1.5)

#Cox proportional hazards model
res.cox <- coxph(data_survival ~ Intensity.minutes.per.day + Pets + Carpet.score.2 +
                   Carpet.score.3 + Carpet.score.4 + Carpet.score.5 + 
                   Carpet.score.6 + Carpet.score.7 + Carpet.score.8 , data=data)
cox.zph(res.cox) #Check the proportional hazards assumption
print(xtable(as.data.frame(cox.zph(res.cox)$table) ), type="latex") #Output above table as latex
summary(res.cox) #Print the summary of the Cox proportional hazards model
print(xtable(as.data.frame(summary(res.cox)$coefficients) ), type="latex") #Output the coefficients table as latex
print(xtable(as.data.frame(summary(res.cox)$conf.int) ), type="latex") #Output the coefficients (CI's) table as latex

####L10 analysis
survbattery = Surv(data$Survival.time, data$Battery.status) # Create survival object
fitbattery = survfit(survbattery ~ 1, data=data) # Construct Kaplan-Meier estimator

battery_sum = summary(fitbattery) # Summarize the Kaplan-Meier estimator
L10_battery = battery_sum$time[which.min(abs(battery_sum$surv - 0.90))] # Find the time at which the survival probability is 0.90
upper_battery = battery_sum$time[which.min(abs(battery_sum$upper - 0.90))] # Find the time at which the upper confidence interval is 0.90

if (L10_battery < upper_battery) {
  print("L10_battery is less than upper_battery")
  print(paste("Value of L10_battery is:", L10_battery))
  print(paste("Value of upper_battery is:", upper_battery))
} else {
  print("L10_battery is equal to upper_battery")
  print(paste("Value of L10_battery is:", L10_battery))
  print(paste("Value of upper_battery is:", upper_battery))
}
