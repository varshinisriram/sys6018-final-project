#-----------------------------------------------------------------------
# SYS 6018 Data Mining Final Project
# Fatality Analysis
# Elena Gillis (emg3sc), Kanika Dawar (kd2hr), Varshini Sriram (vs4vx)
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
# Data Preparation and Exploration
#-----------------------------------------------------------------------

# Setting the working directory to source file location
setwd("C:/Users/Kanika/Downloads/FARS - DM Project/FARS DATA/Data/")

install.packages("h2o")
#install.packages('pROC')
#install.packages("parallelSVM")
#install.packages("Rtsne")
#install.packages("MLmetrics")

# Importing the required packages
library(readr)
library(caret)
library(dplyr)
library (e1071)
library (ROCR)
library(tree)
library (randomForest)
library(MASS)
library(caTools)                
library(h2o)
library(ggplot2)
library(parallelSVM)
library(Rtsne)
library(forecast)
library(tseries)
library(lubridate)
library(doParallel)
library(MLmetrics)
library(gbm)

#-----------------------------
# Load and merge the data
#-----------------------------

# Loading the data
setwd("C:/Users/Kanika/Downloads/FARS - DM Project/FARS DATA/Data/FARS2015NationalCSV")
files15 = list.files(pattern=".*csv")
acc15 = read_csv(files15[1])
per15 = read_csv(files15[2])
veh15 = read_csv(files15[3])

setwd("C:/Users/Kanika/Downloads/FARS - DM Project/FARS DATA/Data/FARS2016NationalCSV")
files16 = list.files(pattern=".*csv")
acc16 = read_csv(files16[1])
per16 = read_csv(files16[2])
veh16 = read_csv(files16[3])

setwd("C:/Users/Kanika/Downloads/FARS - DM Project/FARS DATA/Data/FARS2017NationalCSV")
files17 = list.files(pattern=".*csv")
acc17 = read_csv(files17[1])
per17 = read_csv(files17[2])
veh17 = read_csv(files17[3])

accident = rbind(acc15, acc16, acc17)
vehicle = rbind(veh15, veh16, veh17)
person = rbind(per15, per16, per17)

#-----------------------------
# EXPLORATORY DATA ANALYSIS
#-----------------------------

#------------------
# ACCIDENT DATASET
#------------------

# Shape of the three merged datasets
dim(accident)
# 101533     41

dim(vehicle)
# 154837     16

dim(person)
# 253015     23

# vehicle and person data has more rows

# check number of unique cases in each dataset
nrow(unique(accident[c('YEAR', 'ST_CASE')])) #101533
nrow(unique(vehicle[c('YEAR', 'ST_CASE')])) #101533
nrow(unique(person[c('YEAR', 'ST_CASE')])) #101533
# all datasets have the same number of unique cases over three years
# datasets can be merged on year and case numbers

# years represented in the data
unique(accident$YEAR)
# 2015 2016 2017

# frequency table for the categorical responst variable
table(accident$FATALS)
#     1     2     3     4     5     6     8     9    10    13 
# 94363  5922   919   235    68    21     1     1     1     2 

# summary of variables in accidents data
str(accident)


### check trends in response in relation to some individual regressors

# accidents and day of the week
table(accident$A_DOW) 
#     1     2     3 
# 60009 41335   189 
# more fatal crashes occur on weekdays

# number of fatalities occuring during weekdays
sum(accident$FATALS[accident$A_DOW==1]) #64839
# fatalities on weekends
sum(accident$FATALS[accident$A_DOW==2]) #45390
# more accidents and more fatalities occur on weekdays


# time of the day
table(accident$A_TOD)
#     1     2     3 
# 48774 51996   763
# more fatal crashes occur in the evenings

# number of fatalities during day-time crashes
sum(accident$FATALS[accident$A_TOD==1]) #53093
# fatalities for night crashes
sum(accident$FATALS[accident$A_TOD==2]) #56523
# more accidents and more fatalities during night-time

# rural vs. urban
table(accident$A_RU)
#     1     2     3 
# 47887 52500  1146 
# more urban crashes

# number of fatalities for rural crashes
sum(accident$FATALS[accident$A_RU==1]) #53252
# number of fatalities for urban crashes
sum(accident$FATALS[accident$A_RU==2]) #55968
# more accidents and more fatalities in urban crashes


# number of crashes in relationship to position on the road
table(accident$A_RELRD)
#     1     2     3     4     6 
# 61577  1525  3919 34324   188 
# 1 - on roadway
# 4 - off roadway
# significantly more crashes on roadway

# number of fatalities on roadways
sum(accident$FATALS[accident$A_RELRD==1]) #67560
# number of fatalities off roadways
sum(accident$FATALS[accident$A_RELRD==4]) #36679
# significantly more fatalities on roadways


# number of crashes per manner of collision
table(accident$A_MANCOL)
#     1     2     3     4     5     6     7 
# 62393  7053 10312 18385  2762   381   247 
# 1 - not w/motor vehicle in transport
# 2 - rear end
# 3 - head on
# 4 - angle

# number of fatalities per manner of collision
sum(accident$FATALS[accident$A_MANCOL==1]) #65686
sum(accident$FATALS[accident$A_MANCOL==2]) #7821
sum(accident$FATALS[accident$A_MANCOL==3]) #12686
sum(accident$FATALS[accident$A_MANCOL==4]) #20449
# significantly more fatalities in collisions that are not with
# other motor vehicles in transport


# single vehicle vs multi-vehicle crushes
table(accident$A_CT)
#     1     2     3 
# 58697 35655  7181 

# single vehicle vatalities
sum(accident$FATALS[accident$A_CT==1]) #61636
# two-vehicle fatalities
sum(accident$FATALS[accident$A_CT==2]) #40286
# more than two vehicle fatalities
sum(accident$FATALS[accident$A_CT==3]) #8501
# more fatalities in single vehicle crashes


# number of fatalities per age group of driver
# ages 15-20
sum(accident$FATALS[accident$A_D15_20==1]) #14389
# ages 21-24
sum(accident$FATALS[accident$A_D21_24==1]) #16439
# over 65
sum(accident$FATALS[accident$A_D65PLS==1]) #21369

# cumulatively young drivers and seniors
sum(accident$FATALS[accident$A_D15_20==1])+
  sum(accident$FATALS[accident$A_D21_24==1])+
  sum(accident$FATALS[accident$A_D65PLS==1]) #52197

# other ages
sum(accident$FATALS)-
  (sum(accident$FATALS[accident$A_D15_20==1])+
     sum(accident$FATALS[accident$A_D21_24==1])+
     sum(accident$FATALS[accident$A_D65PLS==1])) #58226
# significant number of fatalities involve young drivers and seniors
# in comparison to other age groups


# create accident by cause df
acc_cause = data.frame(matrix(1,1)) #initalize a dataframe
acc_cause$dist= sum(accident$A_DIST==1) # number of distracted
acc_cause$drowsy = sum(accident$A_DROWSY==1) # number of drowsy
acc_cause$hr = sum(accident$A_HR==1) # hit and run
acc_cause$inter = sum(accident$A_INTER==1) #interstate fatalities
acc_cause$intsec = sum(accident$A_INTSEC==1) #intersection fatalities
acc_cause$junc = sum(accident$A_JUNC==1) #junction accidents
acc_cause$lt = sum(accident$A_LT==1) #large trucks
acc_cause$mc = sum(accident$A_MC==1) #motorcycles
acc_cause$ped = sum(accident$A_PED==1) #pedestrian
acc_cause$pedal = sum(accident$A_PEDAL==1) #pedalcyclist
acc_cause$polpur = sum(accident$A_POLPUR==1) #police persuit
acc_cause$posbac = sum(accident$A_POSBAC==1) #legally intoxicated
acc_cause$rd = sum(accident$A_RD==1) #roadway departure
acc_cause$roll = sum(accident$A_ROLL==1) #rollover
acc_cause$spcra = sum(accident$A_SPCRA==1) #speeding


# delete first observation and transpose acc_cause
acc_cause = data.frame(t(acc_cause[,-1]))
names(acc_cause) = c('Accidents')
acc_cause$Cause = rownames(acc_cause)
acc_cause[2] = NULL

# calculate fatqalities per cause and add to the df
acc_cause['rd',2] = sum(accident$FATALS[accident$A_RD==1])
acc_cause['spcra',2] = sum(accident$FATALS[accident$A_SPCRA==1])
acc_cause['intsec',2] = sum(accident$FATALS[accident$A_INTSEC==1])
acc_cause['junc',2] = sum(accident$FATALS[accident$A_JUNC==1])
acc_cause['roll',2] = sum(accident$FATALS[accident$A_ROLL==1])
acc_cause['posbac',2] = sum(accident$FATALS[accident$A_POSBAC==1])
acc_cause['ped',2] = sum(accident$FATALS[accident$A_PED==1])
acc_cause['mc',2] = sum(accident$FATALS[accident$A_MC==1])
acc_cause['inter',2] = sum(accident$FATALS[accident$A_INTER==1])
acc_cause['lt',2] = sum(accident$FATALS[accident$A_LT==1])
acc_cause['dist',2] = sum(accident$FATALS[accident$A_DIST==1])
acc_cause['hr',2] = sum(accident$FATALS[accident$A_HR==1])
acc_cause['pedal',2] = sum(accident$FATALS[accident$A_PEDAL==1])
acc_cause['drowsy',2] = sum(accident$FATALS[accident$A_DROWSY==1])
acc_cause['polpur',2] = sum(accident$FATALS[accident$A_POLPUR==1])

# rename columns
names(acc_cause) = c('Accidents', 'Fatalities')

# sort dataframe by number of fatalities
acc_cause = as.data.frame(acc_cause[order(acc_cause$Fatalities, decreasing = T),])

# print the dataframe
acc_cause

#        Accidents Fatalities
# rd         52354      57698
# spcra      26712      29731
# roll       24064      26897
# intsec     24909      26796
# junc       24909      26796
# posbac     22839      25423
# ped        17461      17813
# mc         15311      15795
# inter      12709      14282
# lt         11756      13225
# dist        9374      10182
# hr          5699       5879
# pedal       2472       2499
# drowsy      2190       2459
# polpur      1033       1204

# most fatalities happen on roadways, in speeding crashes,
# on junctions and intersections, and involve driving under intoxication

# sort dataframe by number of fatalities
acc_cause = as.data.frame(acc_cause[order(acc_cause$Fatalities, decreasing = T),])

acc_cause$Accidents = as.numeric(acc_cause$Accidents)
acc_cause$Fatalities = as.numeric(acc_cause$Fatalities)

# print the dataframe
acc_cause

#        Accidents Fatalities
# rd         52354      57698
# spcra      26712      29731
# roll       24064      26897
# intsec     24909      26796
# junc       24909      26796
# posbac     22839      25423
# ped        17461      17813
# mc         15311      15795
# inter      12709      14282
# lt         11756      13225
# dist        9374      10182
# hr          5699       5879
# pedal       2472       2499
# drowsy      2190       2459
# polpur      1033       1204

# most fatalities happen on roadways, in speeding crashes,
# on junctions and intersections, and involve driving under intoxication

# add ratio for fatalities per accident in each category
acc_cause$Ratio = acc_cause$Fatalities/acc_cause$Accidents

# sort dataframe by number of fatalities
acc_cause = as.data.frame(acc_cause[order(acc_cause$Ratio, decreasing = T),])
acc_cause

#        Accidents Fatalities    Ratio
# polpur      1033       1204 1.165537
# lt         11756      13225 1.124957
# inter      12709      14282 1.123771
# drowsy      2190       2459 1.122831
# roll       24064      26897 1.117728
# posbac     22839      25423 1.113140
# spcra      26712      29731 1.113020
# rd         52354      57698 1.102074
# dist        9374      10182 1.086196
# intsec     24909      26796 1.075756
# junc       24909      26796 1.075756
# mc         15311      15795 1.031611
# hr          5699       5879 1.031584
# ped        17461      17813 1.020159
# pedal       2472       2499 1.010922

# accidents involving police persuit, large trucks, interstate collision 
# and drowsy drivers had the most fatalities per accident

#------------------
# VEHICLE DATASET
#------------------

hist(accident$FATALS)

# Distribution of vehicle types in accidents

hist(vehicle$A_BODY, xlab = "Vehicle Body", main = "Vehicle Type in Accident", axis(side=1, at=seq(0,9,1), labels=seq(0,9,1)))
legend('topright', c("1 - Passenger Car", "2 - LT Pickup", "3 - LT Utility","4 - LT Van", "5 - LT Other",
                     "6 - Large Truck", "7 - Motorcycle", "8 - Bus", "9 - Other"), cex=.75)

# Passenger Cars formed majority of the fatal accidents

# Distribution of Commercial Driving License validity in accidents

hist(vehicle$A_CDL_S, xlab = "CDL Status", 
     main = "License Type in Accident", axis(side=1, at=seq(0,4,1), labels=seq(0,4,1)))
legend('topright', c("1 - Valid", "2 - Invalid", "3 - Unknown","4 - Not Applicable"), cex=.75)

# We see a heavy skew towards invalid commercial driving licenses in fatal accidents

# Distribution of initial impact types in accidents

hist(vehicle$A_IMP1, xlab = "Initial Impact Point", 
     main = "Initial Impact Point in Accident", axis(side=1, at=seq(0,7,1), labels=seq(0,7,1)))
legend('topright', c("1 - Non Collision", "2 - Front", "3 - Right","4 - Rear",
                     "5 - Left","6 - Other","7 - Unknown"), cex=.75)

# Front Collision initial impact point form majority of fatal accidents

# Distribution of License Compliance in accidents

hist(vehicle$A_LIC_C, xlab = "Licence Compliance", 
     main = "License Compliance in Accident", axis(side=1, at=seq(0,4,1), labels=seq(0,4,1)))
legend('topright', c("1 - Valid", "2 - Invalid", "3 - Unknown","4 - Not Applicable"), cex=.75)

# 10% of the accidents have invalid license compliance to the vehicle being driven

# Distribution of License Status in accidents

hist(vehicle$A_LIC_S, xlab = "Licence Status", 
     main = "License Status in Accident", axis(side=1, at=seq(0,4,1), labels=seq(0,4,1)))
legend('topright', c("1 - Valid", "2 - Invalid", "3 - Unknown","4 - Not Applicable"), cex=.75)

# Around 15% of the Licenses are invalid when it comes to vehicles in fatal accidents

# Distribution of Motorcycle License Status in accidents

plotx = vehicle[!(vehicle$A_MC_L_S == 4),"A_MC_L_S"]
hist(plotx$A_MC_L_S, xlab = "Motorcycle Licence Status", 
     main = "Motorcycle License Status in Accident", axis(side=1, at=seq(0,3,1), labels=seq(0,3,1)))
legend('topright', c("1 - Valid", "2 - Invalid", "3 - Unknown"), cex=.75)

# Almost 50% of the Motor cycle accidents have a vehicle with an invalid license

# Distribution of Speeding Vehicle in accidents

hist(vehicle$A_SPVEH, xlab = "Speeding Vehicle", 
     main = "Speeding Vehicle in Accident", axis(side=1, at=seq(0,3,1), labels=seq(0,3,1)))
legend('topright', c("1 - Speed Involved", "2 - No Speed Involved", "3 - Not defined"), cex=.75)

# Around 25% of the fatal accidents involve speeding

# Distribution of School Buses in accidents

hist(vehicle$A_SBUS, xlab = "School Bus", 
     main = "School Buses in Accident", axis(side=1, at=seq(0,3,1), labels=seq(0,3,1)))
legend('topleft', c("1 - School Bus", "2 - Vehicle used as School Bus", "3 - Other"), cex=.75)

# Statistically insignificant no of school buses involved in fatal accidents

#------------------
# PERSON DATASET
#------------------

# Distribution of Age Category in accidents

hist(person$A_AGE3, xlab = "Age Category", 
     main = "Age Category in Accident", axis(side=1, at=seq(0,13,1), labels=seq(0,13,1)))
legend('topright', c("1 - 0-3", "2 - 4-7", "3 - 8-12","4 - 13-15", "5 - 16-20", "6 - 21-24",
                     "7 - 25-34", "8 - 35-44", "9 - 45-54", "10 - 55-64", "11 - 65-74", "12 - 75+",
                     "13 - Unknown"), cex=.75)

# Skew towards millenials in fatal accidents

# Distribution of Alcohol Detection in accidents

hist(person$A_ALCTES, xlab = "Alcohol Testing", 
     main = "Alcohol Detection in Accident", axis(side=1, at=seq(0,5,1), labels=seq(0,5,1)))
legend('topright', c("1 - No Alcohol", "2 - Positive BAC", "3 - Not Tested","4 - Tested - Unknown",
                     "5 - Unknown if Tested"), cex=.75)

# 50% of the accidents had Alcohol testing done
# Of which, 33% had +ve BAC

# Distribution of No of Persons ejected in accidents

hist(person$A_EJECT, xlab = "Alcohol Testing", 
     main = "Ejection Status in Accident", axis(side=1, at=seq(0,3,1), labels=seq(0,3,1)))
legend('topright', c("1 - Not Ejected", "2 - Ejected", "3 - Unknown"), cex=.75)

# Majority of accidents people were not ejected

# Distribution of Helmet Use in accidents

hist(person$A_HELMUSE, xlab = "Helmet Status", 
     main = "Helmet Status in Accident", axis(side=1, at=seq(0,3,1), labels=seq(0,3,1)))
legend('topright', c("1 - Helmeted", "2 - Not Helmeted", "3 - Unknown"), cex=.75)

# We see that majority of accidents, the person did not have a helmet on
# This skew is because of less no of motorcycle accidents present in the dataset

# Distribution of Hispanic Origins in accidents

hist(person$A_HISP, xlab = "Hispanic Origin", 
     main = "Hispanic origin individuals in Accident", axis(side=1, at=seq(-1,3,1), labels=seq(-1,3,1)))
legend('topright', c("-1 - Not Applicable/Not Available", "1 - Non Hispance", "2 - Hispanic", "3 - Unknown"), cex=.75)

# Statistically insignificant information about Hispanic origin

#----------------------
# SUMMARISING ACCIDENT DATASET
#----------------------

# Getting the number of distinct/unique values in each of the columns in the train dataset
feat_uniq_values <- as.data.frame(sapply(accident, n_distinct))
feat_uniq_values$column = row.names(feat_uniq_values)
names(feat_uniq_values) = c("uniqvalcount", "column")
row.names(feat_uniq_values) = NULL
feat_uniq_values = feat_uniq_values[,c(2,1)]

# Getting the number of missing values in each column
na_values = as.data.frame(sapply(accident, function(x)sum(is.na(x))))
na_values$column = row.names(na_values)
names(na_values) = c("missingvalcount", "column")
row.names(na_values) = NULL
na_values = na_values[,c(2,1)]
#NO missing values in the data

# Accident Data Cleaning
# Removing A_CRAINJ (Crash Injury Type) since we have only fatal crashes in the data (unique value is 1)
accident$A_CRAINJ = NULL

#Keeping only A_D15_20 and A_D21_24 and removing other age variables
accident$A_D15_19 = NULL
accident$A_D16_19 = NULL
accident$A_D16_20 = NULL
accident$A_D16_24 = NULL

# Keeping A_PED_F and A_PEDAL_F and removing A_PED and A_PEDAL
accident$A_PED = NULL
accident$A_PEDAL = NULL

# Converting categorical variables to factors
fact_cols = colnames(accident)
fact_cols = fact_cols[-c(1, 3, 5)]
accident[fact_cols] <- lapply(accident[fact_cols], factor)

# One Hot Encoding of the factor variables
dmy <- dummyVars(" ~ .", data = accident, sep = "_", fullRank = T)
accident_new <- data.frame(predict(dmy, newdata = accident))

#----------------------
# SUMMARISING VEHICLE DATASET
#----------------------

# Converting categorical variables to factor

vehicle$A_BODY <- as.factor(vehicle$A_BODY)

vehicle$A_CDL_S <- as.factor(vehicle$A_CDL_S)

vehicle$A_DRDIS <-  as.factor(vehicle$A_DRDIS)

vehicle$A_DRDRO <- as.factor(vehicle$A_DRDRO)

vehicle$A_IMP1 <- as.factor(vehicle$A_IMP1)

vehicle$A_IMP2 <- as.factor(vehicle$A_IMP2)

vehicle$A_LIC_C <- as.factor(vehicle$A_LIC_C)

vehicle$A_LIC_S <- as.factor(vehicle$A_LIC_S)

vehicle$A_MC_L_S <- as.factor(vehicle$A_MC_L_S)

vehicle$A_SBUS <-  as.factor(vehicle$A_SBUS)

vehicle$A_SPVEH <- as.factor(vehicle$A_SPVEH)

vehicle$A_VROLL <- as.factor(vehicle$A_VROLL)

# IMP_2 is NULL for everything, removing it

vehicle$A_IMP2 <- NULL

# Also, removing vehicle number
vehicle$VEH_NO <- NULL

# One-hot encoding the factor variables
dmy <- dummyVars(" ~ .", data = vehicle,sep = "_",fullRank = T)
vehicle_new <- data.frame(predict(dmy, newdata = vehicle))

# Summarizing data on (year, Case No) level
vehicle_identifier <- group_by(vehicle_new, YEAR, ST_CASE)

# Taking avg model year of the cars in the accident
model <- summarize(vehicle_identifier, col = mean(A_MOD_YR))

# Taking sum for all the one hot encoded factor variables
vehicle_unique = aggregate(. ~ YEAR+ST_CASE, vehicle_new[,-31], sum)

# changing car model to avg
vehicle_unique$A_MOD_YR <- model$col

# Taking no of vehicles in the accident
model <- summarize(vehicle_identifier, col = n())
vehicle_unique[,'NO_OF_VEHICLES'] <- model$col

# Distribution of number of vehicles in accidents

plotx = vehicle_unique[vehicle_unique$NO_OF_VEHICLES < 10,"NO_OF_VEHICLES"]

hist(plotx , xlab = "No of vehicles", main = "No of vehicles in Accident", xlim = c(0,10 ),
     axis(side=1, at=seq(0,10,1), labels=seq(0,10,1)))


#----------------------
# SUMMARISING PERSON DATASET
#----------------------

# keeping only AGE3 for age
person = person[, -c(1,2,4,5,6,7,8,9)]

person$VEH_NO <- NULL
person$PER_NO = NULL

# all variables apart form YEAR and ST_CASE need to be factors
# make a list of categorical variables
person_variables = colnames(person[,-c(2,3)])

# change categorical variables to factors
person[person_variables] = lapply(person[person_variables], factor) 

# check classes of all the variables
sapply(person, class)

#   A_AGE3   ST_CASE    VEH_NO    PER_NO      YEAR   A_PTYPE A_RESTUSE A_HELMUSE 
# "factor"  "integer"  "factor"  "factor"  "integer"  "factor"  "factor"  "factor" 
# A_ALCTES    A_HISP    A_RCAT   A_HRACE   A_EJECT  A_PERINJ     A_LOC 
# "factor"  "factor"  "factor"  "factor"  "factor"  "factor"  "factor" 



# One-hot encoding for factor variables
dmy <- dummyVars(" ~ .", data = person, sep = "_", fullRank = T)
person_new <- data.frame(predict(dmy, newdata = person))

# Summarizing data on (year, Case No) level
person_identifier <- group_by(person_new, YEAR, ST_CASE)

# count number of entries in each category per case 
person_new = aggregate(. ~ YEAR + ST_CASE, person_new, sum)

# Taking no of persons in the accident

model <- summarize(person_identifier, col = n())
person_new[,'NO_OF_PERSONS'] <- model$col

# Distribution of number of persons in accidents

plotx = person_new[person_new$NO_OF_PERSONS < 20,"NO_OF_PERSONS"]

hist(plotx, xlab = "No of persons", main = "No of persons in Accident", xlim = c(0,20 ),
     axis(side=1, at=seq(0,20,1), labels=seq(0,20,1)))

#----------------------
# MERGING ACCIDENT, PERSON, AND VEHICLE DATASET
#----------------------

data = merge(x = accident_new, y = person_new, by=c('YEAR','ST_CASE'), all.x = TRUE)
data = merge(x = data, y = vehicle_unique, by=c('YEAR','ST_CASE'), all.x = TRUE)


any(is.na(data))
# NO missing values in the data

# Understanding the distribution of fatals
fatals = as.data.frame(table(data$FATALS))
names(fatals) = c("Fatals", "Percent")
fatals$Percent = 100*fatals$Percent/sum(fatals$Percent)
#93% of fatalities involve 1 fatal, 6% of fatalities involve 2 fatals and the rest 1% of fatalities involve more than 2 fatals

# Defining accident severity - low, medium, high based on number of fatalities
# low - 1 fatal
# medium - 2 fatals
# high - > 2 fatals
# Converting to a classification problem
data$severity = character(length = nrow(data))
data$severity[data$FATALS == 1] = "low"
data$severity[data$FATALS == 2] = "medium"
data$severity[data$FATALS > 2] = "high"
data$severity = as.factor(data$severity)

# Storing severity (response), fatalities, year, st_case separately and removing it from the dataset
severity = data$severity
data$severity = NULL
year = data$YEAR
data$YEAR = NULL
st_case = data$ST_CASE
data$ST_CASE = NULL
fatalities = data$FATALS
data$FATALS = NULL

#----------------------
# Visualizing data in 2D : t-SNE and PCA
#----------------------

# t-sne visualization of the data
library(Rtsne)
set.seed(10)
tsne_result = Rtsne(data)
tsne_res = as.data.frame(tsne_result$Y)
tsne_res$severity = severity
save(tsne_res, file = "tsne.RData")

# Plotting the t-sne output
library(ggplot2)
ggplot(tsne_res, aes(x = V1, y = V2, color = severity)) + geom_point() + ggtitle("First and Second tSNE dimensions colored by severity") + theme(plot.title = element_text(hjust = 0.5))

# pca visualization of the data                                 
pca_result = prcomp(data, rank. = 2)
pca_res = as.data.frame(pca_result$x)
pca_res$severity = severity
save(pca_res, file = "pca.RData")

# Plotting the pca output                                 
ggplot(pca_res, aes(x = PC1, y = PC2, color = severity)) + geom_point() + ggtitle("First and Second PCA dimensions colored by severity") + theme(plot.title = element_text(hjust = 0.5))


#-----------------------------------------------------------------------
# Accident severity Classification
#-----------------------------------------------------------------------

data$severity <- severity

#setwd("C:/Users/Kanika/Downloads/FARS - DM Project/FARS DATA/Data/")

write_csv(data, "final_data.csv")

data <- read_csv("final_data.csv")

data$severity <- as.factor(data$severity)

#--------------------------
# VARIABLE SELECTION using RF
#--------------------------

# Removing county and region
data <- data [, -c(51:365)]

# DECIDING WHAT VARIABLES TO KEEP USING RANDOM FOREST by h2o
y.col <- 'severity'
x.cols <- setdiff(colnames(data), y.col)

# We split train set for CV
set.seed(1)

sample = sample.split(data$severity, SplitRatio = .5)
sub_train = subset(data, sample == TRUE)
sub_test  = subset(data, sample == FALSE)

h2o.init(nthreads=-1,max_mem_size='10G')
sub_train = as.h2o(sub_train)
sub_test = as.h2o(sub_test)
RF_model = h2o.randomForest(x=x.cols,
                            y=y.col,
                            ntrees = 50,
                            mtries = -1,
                            max_depth=50,
                            nfolds = 10,
                            training_frame=sub_train,
                            validation_frame=sub_test,
                            seed=1)

# RF_model will give you summary of RF model with Cross validation information on test(originally derived from train set)
variable_rf <-  as.data.frame (h2o.varimp (RF_model))

imp_variables <- variable_rf[variable_rf$scaled_importance > 0.002,"variable"]

imp_variables

# [1] "A_HISP_1"      "A_RCAT_1"      "A_HRACE_2"     "A_HRACE_1"     "A_RCAT_2"      "A_HISP_2"      "A_HISP_3"      "A_HRACE_3"    
# [9] "A_RCAT_8"      "A_HRACE_9"     "A_PTYPE_2"     "A_PERINJ_6"    "A_HRACE_8"     "A_HELMUSE_2"   "A_ALCTES_2"    "A_RESTUSE_2"  
# [17] "A_RCAT_7"      "A_RCAT_3"      "A_HRACE_4"     "A_EJECT_2"     "A_HRACE_5"     "A_RCAT_4"      "A_AGE3_7"      "A_MANCOL_3"   
# [25] "A_ALCTES_5"    "A_ALCTES_3"    "A_IMP1_2"      "A_AGE3_5"      "A_AGE3_6"      "A_HELMUSE_3"   "A_PTYPE_3"     "A_DRDIS_2"    
# [33] "A_RESTUSE_3"   "A_AGE3_8"      "A_MC_L_S_4"    "A_CDL_S_2"     "A_POSBAC_3"    "A_AGE3_9"      "A_SBUS_3"      "A_AGE3_10"    
# [41] "A_RU_2"        "A_BODY_3"      "NO_OF_PERSONS" "A_SPVEH_2"     "A_LOC_3"       "A_ROLL_2"      "A_VROLL_2"     "A_BODY_2"     
# [49] "A_TOD_2"       "A_SPCRA_2"     "A_LIC_S_2"     "A_AGE3_2"      "A_DOW_2"       "A_JUNC_2"      "A_POSBAC_2"    "A_RD_2"       
# [57] "A_LIC_C_2"     "A_ROADFC_3"    "A_PED_F_2"    

imp_variables <- c("A_HISP_1","A_RCAT_1","A_HRACE_2","A_HRACE_1","A_RCAT_2","A_HISP_2","A_HISP_3","A_HRACE_3",
                   "A_RCAT_8","A_HRACE_9","A_PTYPE_2","A_PERINJ_6","A_HRACE_8","A_HELMUSE_2","A_ALCTES_2","A_RESTUSE_2",
                   "A_RCAT_7","A_RCAT_3","A_HRACE_4","A_EJECT_2","A_HRACE_5","A_RCAT_4","A_AGE3_7","A_MANCOL_3",
                   "A_ALCTES_5","A_ALCTES_3","A_IMP1_2","A_AGE3_5","A_AGE3_6","A_HELMUSE_3","A_PTYPE_3","A_DRDIS_2",
                   "A_RESTUSE_3","A_AGE3_8","A_MC_L_S_4","A_CDL_S_2","A_POSBAC_3","A_AGE3_9","A_SBUS_3","A_AGE3_10",
                   "A_RU_2","A_BODY_3","NO_OF_PERSONS","A_SPVEH_2","A_LOC_3","A_ROLL_2","A_VROLL_2","A_BODY_2",
                   "A_TOD_2","A_SPCRA_2","A_LIC_S_2","A_AGE3_2","A_DOW_2","A_JUNC_2","A_POSBAC_2","A_RD_2",
                   "A_LIC_C_2","A_ROADFC_3","A_PED_F_2")


#---------------------------------------
# SUPPORT VECTOR MACHINES
#---------------------------------------
#--------------------------
# SVM - linear kernel
#--------------------------

# subsetting data for SVM
sub_train_sv <- data[,c(imp_variables, "severity")]

pkgs <- c('foreach', 'doParallel')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)


set.seed(2016)
sub_train_sv$fold <- caret::createFolds(1:nrow(sub_train_sv), k = 4, list = FALSE)

### PARAMETER LIST ###
cost <- c(0.001, 0.01, 0.1, 1, 5, 10, 100)
gamma <- c(0.5,1,2,3,4)
parms <- expand.grid(cost = cost, gamma = gamma)

### LOOP THROUGH PARAMETER VALUES ###
result <- foreach(i = 1:nrow(parms), .combine = rbind) %do% {
  c <- parms[i, ]$cost
  
  ### K-FOLD VALIDATION ###
  
  out <- foreach(j = 1:max(sub_train_sv$fold), .combine = rbind, .inorder = FALSE) %dopar% {
    deve <- sub_train_sv[sub_train_sv$fold != j, ]
    test <- sub_train_sv[sub_train_sv$fold == j, ]
    mdl <- e1071::svm(severity~., data = deve, type = "C-classification", kernel = "linear", cost = c, probability = TRUE)
    pred <- predict(mdl, test, decision.values = TRUE, probability = TRUE)
    data.frame(y = test$severity, prob = attributes(pred)$probabilities[, 2])
  }
  
  ### CALCULATE SVM PERFORMANCE ###
  roc <- pROC::roc(as.factor(out$y), out$prob)
  data.frame(parms[i, ], roc = roc$auc[1])
}


dat <- data.frame("cost" = as.factor(result$cost),
                  "gamma" = as.factor(result$gamma),
                  "AUC" = result$roc)

# plotting heat map for different cost and gamma values AUC
ggplot(dat, aes(cost, gamma))  + geom_tile(aes(fill=AUC), colour = "white")  + scale_fill_gradient(low = "white",  high = "steelblue") + geom_text(label = round((dat$AUC),2))

#--------------------------
# SVM - radial kernel
#--------------------------

# We split train set for CV
set.seed(1)

# splitting data into test and train set

sample = sample.split(data$severity, SplitRatio = .8)

sub_train = subset(data, sample == TRUE)
sub_test  = subset(data, sample == FALSE)

# keeping only important variables decided by RF
sub_train <- sub_train[,c(imp_variables, "severity")]
sub_test <- sub_test[,c(imp_variables, "severity")]

# Running parallel SVM on default cost and gamma
parallelSvm1 <- parallelSVM(severity ~ ., data = sub_train, kernel = 'radial',
                            numberCores = 8, 
                            probability = TRUE,
                            decision.values =T)

fitted.svm1 = predict (parallelSvm1, sub_train, decision.values = TRUE)
predict.svm1 = predict (parallelSvm1, sub_test, decision.values = TRUE)

# FOR THE TRAINING SET
confusionMatrix(sub_train$severity, fitted.svm1)

# Confusion Matrix and Statistics
# 
# Reference
#         high   low medium
# high     746    96    156
# low        0 75490      0
# medium     1   203   4534
# 
# Overall Statistics
# 
# Accuracy : 0.9944          
# 95% CI : (0.9938, 0.9949)
# No Information Rate : 0.9331          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9566          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Statistics by Class:
# 
# Class: high Class: low Class: medium
# Sensitivity             0.998661     0.9961       0.96674
# Specificity             0.996869     1.0000       0.99733
# Pos Pred Value          0.747495     1.0000       0.95694
# Neg Pred Value          0.999988     0.9479       0.99796
# Prevalence              0.009197     0.9331       0.05774
# Detection Rate          0.009184     0.9294       0.05582
# Detection Prevalence    0.012287     0.9294       0.05833
# Balanced Accuracy       0.997765     0.9980       0.98204
# plotting roc curve for test vs train

# FOR THE TESTING SET
confusionMatrix(sub_test$severity, predict.svm1)

Severity_level_actual <- factor(c("high", "high", "high", "low", "low", "low", "medium", "medium", "medium"))
Severity_Level_predicted <- factor(c("high", "low", "medium", "high", "low", "medium", "high", "low", "medium"))

Y <- c(171, 22, 57, 0, 18873, 0, 0, 45, 1139)

df <- data.frame(Severity_level_actual, Severity_Level_predicted, Y)

ggplot(data =  df, mapping = aes(x = Severity_level_actual, y = Severity_Level_predicted)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  theme_bw() + theme(legend.position = "none")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  high   low medium
# high     171    22     57
# low        0 18873      0
# medium     0    45   1139
# 
# Overall Statistics
# 
# Accuracy : 0.9939          
# 95% CI : (0.9927, 0.9949)
# No Information Rate : 0.9327          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9529          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Statistics by Class:
# 
# Class: high Class: low Class: medium
# Sensitivity             1.000000     0.9965       0.95234
# Specificity             0.996077     1.0000       0.99765
# Pos Pred Value          0.684000     1.0000       0.96199
# Neg Pred Value          1.000000     0.9533       0.99702
# Prevalence              0.008421     0.9327       0.05890
# Detection Rate          0.008421     0.9294       0.05609
# Detection Prevalence    0.012311     0.9294       0.05831
# Balanced Accuracy       0.998038     0.9982       0.97499

#---------------------------------------
# KNN MODEL
#---------------------------------------

data_new = data[,c(imp_variables, "severity")]

# Partitioning the data into training and validation data
set.seed(100)
index = createDataPartition(data_new$severity, p = 0.7, list = F)
train = data_new[index,]
validation = data_new[-index,]

# Setting up train controls
repeats = 3
numbers = 10
tunel = 10

# create a cluster running on 8 cores
cluster = makeCluster(8)
registerDoParallel(cluster)

set.seed(1234)
x = trainControl(method = "repeatedcv", number = numbers, repeats = repeats, classProbs = TRUE, summaryFunction = multiClassSummary)

knn.model <- train(severity ~. , data = train, method = "knn",
                   trControl = x,
                   metric = "ROC",
                   tuneLength = tunel)

# tear down cluster
stopCluster(cluster)

# Summary of model
knn.model

# k-Nearest Neighbors 
# 
# 71075 samples
# 59 predictor
# 3 classes: 'high', 'low', 'medium' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 3 times) 
# Summary of sample sizes: 63967, 63968, 63968, 63967, 63967, 63967, ... 
# Resampling results across tuning parameters:
#   
#   k   logLoss    AUC        prAUC      Accuracy   Kappa      Mean_F1    Mean_Sensitivity  Mean_Specificity
# 5  0.5383000  0.8976070  0.4511327  0.9539782  0.5306489  0.6419451  0.5531600         0.8117928       
# 7  0.4473077  0.9177019  0.4755909  0.9508406  0.4843720  0.6113800  0.5263062         0.7973954       
# 9  0.3845460  0.9314439  0.4933009  0.9486223  0.4504280  0.5844005  0.5044249         0.7874983       
# 11  0.3453900  0.9401302  0.5025080  0.9468824  0.4222073  0.5646908  0.4887714         0.7791644       
# 13  0.3155998  0.9472012  0.5104747  0.9455692  0.4001474  0.5515098  0.4783824         0.7727622       
# 15  0.2919252  0.9523602  0.5159083  0.9445468  0.3825072  0.5405725  0.4700362         0.7677526       
# 17  0.2776536  0.9555951  0.5194434  0.9433650  0.3622211  0.5256483  0.4590605         0.7623180       
# 19  0.2671895  0.9581169  0.5225075  0.9424082  0.3451361  0.5148029  0.4511794         0.7577202       
# 21  0.2541708  0.9608345  0.5250564  0.9415406  0.3290456  0.5042306  0.4437145         0.7533594       
# 23  0.2430859  0.9635627  0.5275932  0.9407808  0.3150601  0.4946660  0.4370647         0.7497533       
# Mean_Pos_Pred_Value  Mean_Neg_Pred_Value  Mean_Precision  Mean_Recall  Mean_Detection_Rate  Mean_Balanced_Accuracy
# 0.9054637            0.9834196            0.9054637       0.5531600    0.3179927            0.6824764             
# 0.8943097            0.9827212            0.8943097       0.5263062    0.3169469            0.6618508             
# 0.8845130            0.9821234            0.8845130       0.5044249    0.3162074            0.6459616             
# 0.8784441            0.9817879            0.8784441       0.4887714    0.3156275            0.6339679             
# 0.8734434            0.9815371            0.8734434       0.4783824    0.3151897            0.6255723             
# 0.8684397            0.9812053            0.8684397       0.4700362    0.3148489            0.6188944             
# 0.8616371            0.9808205            0.8616371       0.4590605    0.3144550            0.6106892             
# 0.8557513            0.9805107            0.8557513       0.4511794    0.3141361            0.6044498             
# 0.8505619            0.9802308            0.8505619       0.4437145    0.3138469            0.5985369             
# 0.8450545            0.9799851            0.8450545       0.4370647    0.3135936            0.5934090             
# 
# logLoss was used to select the optimal model using the smallest value.
# The final value used for the model was k = 23.

# Using logLoss to choose the optimal model instead of accuracy because of severe class imbalances

plot(knn.model)

# Validation
valid_pred = predict(knn.model, validation)

# Model Performance Scores
confusionMatrix(valid_pred, validation$severity)

Severity_level_actual <- factor(c("high", "high", "high", "low", "low", "low", "medium", "medium", "medium"))
Severity_Level_predicted <- factor(c("high", "low", "medium", "high", "low", "medium", "high", "low", "medium"))

Y <- c(57, 106, 211, 0, 28308, 0, 0, 1489, 287)

df <- data.frame(Severity_level_actual, Severity_Level_predicted, Y)

ggplot(data =  df, mapping = aes(x = Severity_level_actual, y = Severity_Level_predicted)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  theme_bw() + theme(legend.position = "none")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  high   low medium
# high      57     0      0
# low      106 28308   1489
# medium   211     0    287
# 
# Overall Statistics
# 
# Accuracy : 0.9407         
# 95% CI : (0.938, 0.9433)
# No Information Rate : 0.9294         
# P-Value [Acc > NIR] : 1.583e-15      
# 
# Kappa : 0.3149         
# Mcnemar's Test P-Value : < 2.2e-16      

#Classification Accuracy for KNN: 94.07%

#---------------------------------------
# RANDOM FOREST MODEL
#---------------------------------------

#setwd("C:/Users/Kanika/Downloads/FARS - DM Project/FARS DATA/Data/")
data <- read_csv("final_data.csv")

data$severity <- as.factor(data$severity)

# Removing county and region
data <- data [, -c(51:365)]

set.seed(123)
sub = sample(1:nrow(data), nrow(data)/2)
sub_train = data[sub,]
sub_test = data[-sub,]

y.col <- 'severity'
x.cols <- setdiff(colnames(data), y.col)

h2o.init(nthreads=-1,max_mem_size='10G')
sub_train = as.h2o(sub_train)
sub_test = as.h2o(sub_test)
RF_model = h2o.randomForest(x=x.cols,
                            y=y.col,
                            ntrees = 50,
                            mtries = -1,
                            max_depth=100,
                            nfolds = 10,
                            training_frame=sub_train,
                            validation_frame=sub_test,
                            seed=1)


# create a dataframe with variable importance
var = as.data.frame(h2o.varimp(RF_model))

# create an array of variable names important for classifying severity
imp_var = var[var$scaled_importance > 0.001,]$variable #108 variables

# make a dataframe with variables identified as important
imp_data = data[,imp_var]

# save the data to csv
write.csv(imp_data, file='rf_imp_data.csv')

# add severity column to the data
imp_data$severity = data$severity

# rerun random forest on selected (important) variables
set.seed(432)
sub = sample(1:nrow(imp_data), nrow(imp_data)/2)
sub_train = imp_data[sub,]
sub_test = imp_data[-sub,]

y.col <- 'severity'
x.cols <- setdiff(colnames(imp_data), y.col)

h2o.init(nthreads=-1,max_mem_size='10G')
sub_train = as.h2o(sub_train)
sub_test = as.h2o(sub_test)
RF_model = h2o.randomForest(x=x.cols,
                            y=y.col,
                            ntrees = 50,
                            mtries = -1,
                            max_depth=100,
                            nfolds = 10,
                            training_frame=sub_train,
                            validation_frame=sub_test,
                            seed=1)

# classification accuracy
cm.rf = as.matrix(h2o.confusionMatrix(RF_model)[1:3,1:3])
cm.rf

#        high   low medium
# high    548     0     36
# low       0 47230      0
# medium    0    61   2891

Severity_level_actual <- factor(c("high", "high", "high", "low", "low", "low", "medium", "medium", "medium"))
Severity_Level_predicted <- factor(c("high", "low", "medium", "high", "low", "medium", "high", "low", "medium"))

Y <- c(548, 0, 0, 0, 47230, 61, 36, 0, 2891)

df <- data.frame(Severity_level_actual, Severity_Level_predicted, Y)

ggplot(data =  df, mapping = aes(x = Severity_level_actual, y = Severity_Level_predicted)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  theme_bw() + theme(legend.position = "none")

# classification accuracy for random forest
sum(diag(cm.rf))/sum(cm.rf)
# 99.81% classification accuracy


# BOOSTING

set.seed(12)
sub = sample(1:nrow(data), nrow(data)/2)
sub_train = data[sub,]
sub_test = data[-sub,]

# fit the boosted model
boost.data = gbm(severity~., data=sub_train, distribution="multinomial", n.trees=50, interaction.depth=4)

# use the boosted model for prediction
yhat.boost = predict(boost.data, newdata=sub_test, n.trees=50)

# find classification with max prediction value
p.yhat.boost = colnames(yhat.boost)[apply(yhat.boost, 1, which.max)]

# create confusion matrix to test accuracy
cm.boost = as.matrix(table(Actual = sub_test$severity, Predicted = p.yhat.boost))
cm.boost

#         Predicted
# Actual  high   low medium
# high     554     0     47
# low        0 47186      0
# medium     0    44   2936

Severity_level_actual <- factor(c("high", "high", "high", "low", "low", "low", "medium", "medium", "medium"))
Severity_Level_predicted <- factor(c("high", "low", "medium", "high", "low", "medium", "high", "low", "medium"))

Y <- c(554, 0, 0, 0, 47186, 44, 47, 0, 2936)

df <- data.frame(Severity_level_actual, Severity_Level_predicted, Y)

ggplot(data =  df, mapping = aes(x = Severity_level_actual, y = Severity_Level_predicted)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  theme_bw() + theme(legend.position = "none")

# classification accuracy for boosting
sum(diag(cm.boost))/sum(cm.boost)
# 99.82% classification accuracy

#-----------------------------------------------------------------------
# Time Series Forecasting of Total Fatal Crashes
#-----------------------------------------------------------------------

# Loading the data from year 1982-2017
setwd('..')
fatalcrashes = read_csv("timeseriesdata.csv")
fatalcrashes$Year = as.Date(as.character(fatalcrashes$Year), format = "%Y")
fatalcrashes$Year <- year(fatalcrashes$Year)
fatalcrashes$fc_ts = ts(fatalcrashes$`Total Fatal Crashes`, start = 1982, end = 2017)

# Plotting the time series
plot.ts(fatalcrashes$fc_ts)

# Checking for stationary time series using ADF Test
# Series should be stationary for ARIMA Model
adf.test(fatalcrashes$fc_ts, alternative = "stationary")

# Augmented Dickey-Fuller Test
# 
# data:  fatalcrashes$fc_ts
# Dickey-Fuller = -2.6784, Lag order = 3, p-value = 0.3092
# alternative hypothesis: stationary

# Fitting an automated ARIMA Model

# Subsetting data for train
train = fatalcrashes[1:34,]

arima.fit <- auto.arima(train$fc_ts, seasonal = FALSE)
arima.fit

# Series: train$fc_ts 
# ARIMA(1,1,0) 
# 
# Coefficients:
#   ar1
# 0.4019
# s.e.  0.1675
# 
# sigma^2 estimated as 1563153:  log likelihood=-281.73
# AIC=567.46   AICc=567.86   BIC=570.46

# Evaluating model residuals and ACF/PACF plots
tsdisplay(residuals(arima.fit))

# Forecasting using the fitted model
fcast = forecast(arima.fit, 2)
# Plotting the forecast
plot(fcast)
# Testing and computing RMSE
fvalues = as.data.frame(fcast$mean)
names(fvalues) = "forcast"
fvalues$actual = fatalcrashes$`Total Fatal Crashes`[35:36]
sqrt(mean((fvalues$actual - fvalues$forcast)^2))
#[1] 885.0426

# Fitting the model for the whole data
arima.fit <- auto.arima(fatalcrashes$fc_ts, seasonal = FALSE)
arima.fit

# Series: fatalcrashes$fc_ts 
# ARIMA(1,1,0) 
# 
# Coefficients:
#   ar1
# 0.4009
# s.e.  0.1531
# 
# sigma^2 estimated as 1571244:  log likelihood=-298.92
# AIC=601.84   AICc=602.22   BIC=604.95

# Evaluating model residuals and ACF/PACF plots
tsdisplay(residuals(arima.fit))

# Forecasting using the fitted model
fcast = forecast(arima.fit, 1)
# Plotting the forecast
plot(fcast)
fcast
#       Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2018        34046.16 32439.74 35652.57 31589.35 36502.96

# The total fatal crashes will decrease to 34046 for the year 2018 according to the prediction made by the ARIMA model
