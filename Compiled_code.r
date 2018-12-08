#-----------------------------------------------------------------------
# SYS 6018 Data Mining Final Project
# Fatality Analysis
# Elena Gillis (emg3sc), Kanika Dawar (kd2hr), Varshini Sriram (vs4vx)
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
# Data Preparation and Exploration
#-----------------------------------------------------------------------

# Setting the working directory to source file location
setwd("C:/Users/Elena/Desktop/MSDS/MSDS Courses/Fall 18/SYS6018/Final Project")

#-----------------------------------------------------------------------
# Load and merge the data
#-----------------------------------------------------------------------

# Importing the required packages
library(readr)
library(caret)

# Loading the data
acc15 = read_csv('./Data/FARS2015NationalCSV/acc_aux.csv')
veh15 = read_csv('./Data/FARS2015NationalCSV/veh_aux.csv')
per15 = read_csv('./Data/FARS2015NationalCSV/per_aux.csv')
acc16 = read_csv('./Data/FARS2016NationalCSV/acc_aux.csv')
veh16 = read_csv('./Data/FARS2016NationalCSV/veh_aux.csv')
per16 = read_csv('./Data/FARS2016NationalCSV/per_aux.csv')
acc17 = read_csv('./Data/FARS2017NationalCSV/acc_aux.csv')
veh17 = read_csv('./Data/FARS2017NationalCSV/veh_aux.csv')
per17 = read_csv('./Data/FARS2017NationalCSV/per_aux.csv')

# combining datasets for each year
accident = rbind(acc15, acc16, acc17)
vehicle = rbind(veh15, veh16, veh17)
person = rbind(per15, per16, per17)



#-----------------------------------------------------------------------
# EXPLORATORY DATA ANALYSIS
#-----------------------------------------------------------------------

#------------------
# ACCIDENT DATASET
#------------------

# Shape of the three merged datasets
dim(accident)
# 101533     41

dim(vehicle)
# 154837     16

dim(person)
# 253015     15

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


#------------------
# VEHICLE DATASET
#------------------

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

#-----------------------------------------------------------------------
# SUMMARISING ACCIDENT DATASET
#-----------------------------------------------------------------------

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

#-----------------------------------------------------------------------
# SUMMARISING VEHICLE DATASET
#-----------------------------------------------------------------------

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


#-----------------------------------------------------------------------
# SUMMARISING PERSON DATASET
#-----------------------------------------------------------------------

# keeping only AGE3 for age
person = person[, -c(1,2,4,5,6,7,8,9)]

# all variables apart form YEAR and ST_CASE need to be factors
# make a list of categorical variables
person_variables = colnames(person[,-c(2,5)])

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

#-----------------------------------------------------------------------
# MERGING ACCIDENT, PERSON, AND VEHICLE DATASET
#-----------------------------------------------------------------------

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
 
#-----------------------------------------------------------------------
# Visualizing data in 2D : t-SNE and PCA
#-----------------------------------------------------------------------

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
                               
