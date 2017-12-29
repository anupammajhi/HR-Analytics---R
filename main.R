
# Clear All Variables
rm(list=ls())

#================= INITIAL STEPS ====================

# Import Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(MASS)
library(car)
library(e1071)
library(caret)
library(caTools)
library(ROCR)
library(DAAG)


# Import Data Files
emp_survey <- read.csv("employee_survey_data.csv", stringsAsFactors = T)
gen_data <- read.csv("general_data.csv",stringsAsFactors = T)
in_time <- read.csv("in_time.csv",stringsAsFactors = T)
out_time <- read.csv("out_time.csv",stringsAsFactors = T)
manager_survey <- read.csv("manager_survey_data.csv",stringsAsFactors = T)

# Function to replace NA by mean
replace_NA_by_mean <- function(DFcolumn){
  DFcolumn[is.na(DFcolumn)] <- round(mean(DFcolumn[!is.na(DFcolumn)]))
  DFcolumn
}


#==================== DATA CLEANING AND PROCESSING ======================

# Checking if Rows are duplicated
  length(unique(gen_data$EmployeeID)) # 4410
  length(unique(emp_survey$EmployeeID)) # 4410
  length(unique(manager_survey$EmployeeID)) # 4410
  length(unique(in_time$X)) # 4410
  length(unique(out_time$X)) # 4410

# Checking if employee ID order is correct in all data frames so that they can be merged/cbind  
  setdiff(gen_data$EmployeeID,emp_survey$EmployeeID)
  setdiff(gen_data$EmployeeID,manager_survey$EmployeeID)
  setdiff(gen_data$EmployeeID,in_time$X)
  setdiff(gen_data$EmployeeID,out_time$X)
  # All have a diff of 0 which indicate them having same IDs
  
# emp_survey : Data Cleaning and Processing

  # Check for NA
  sapply(emp_survey, function(x){sum(is.na(x))}) # NA Values found; EnvironMentalSatisfaction:25, JobSatisfaction:20, WorkLifeBalance:38
  # Replacing with Mean : Since the values are only limited to 1,2,3,4 and very few NA are there 
  emp_survey$EnvironmentSatisfaction <- replace_NA_by_mean(emp_survey$EnvironmentSatisfaction)
  emp_survey$JobSatisfaction <- replace_NA_by_mean(emp_survey$JobSatisfaction)
  emp_survey$WorkLifeBalance <- replace_NA_by_mean(emp_survey$WorkLifeBalance)
  
  # Check Data validity
  sapply(emp_survey, function(x){levels(as.factor(x))}) # Levels are OK and within limit  as mentioned in dictionary, No invalid data
    
# manager_survey : Data Cleaning and Processing
  
  # Check for NA
  sapply(manager_survey, function(x){sum(is.na(x))}) # No NA Values 
  # Check Data Validity
  sapply(manager_survey, function(x){levels(as.factor(x))}) # Levels are OK and within limit as mentioned in dictionary, No invalid data
  
  
# gen_data : Data Cleaning and Processing
  
  # Check for NA
  sapply(gen_data, function(x){sum(is.na(x))}) # NA values found; NumCompaniesWorked:19, TotalWorkingYears:9
  gen_data[which(is.na(gen_data$NumCompaniesWorked)),]
  gen_data[which(is.na(gen_data$TotalWorkingYears)),]
  
  # Imputing NA values with median NumCompaniesWorked based on TotalWorkingYears
  # To achieve this we take the value of TotalWorkingYears for the corresponding row of NA NumCompaniesWorked and compute the median of NumCompaniesWorked with all rows having same TotalWorkingYears
  gen_data[which(is.na(gen_data$NumCompaniesWorked)),"NumCompaniesWorked"] <- sapply(gen_data[which(is.na(gen_data$NumCompaniesWorked)),"TotalWorkingYears"], function(x){
    round(median(gen_data$NumCompaniesWorked[which(gen_data$TotalWorkingYears == x)],na.rm = T))
  })
  
  # Imputing NA values with mean TotalWorkingYears based on NumCompaniesWorked
  # To achieve this we take the value of NumCompaniesWorked for the corresponding row of NA TotalWorkingYears and compute the median of TotalWorkingYears with all rows having same NumCompaniesWorked
  gen_data[which(is.na(gen_data$TotalWorkingYears)),"TotalWorkingYears"] <- sapply(gen_data[which(is.na(gen_data$TotalWorkingYears)),"NumCompaniesWorked"], function(x){
    round(median(gen_data$TotalWorkingYears[which(gen_data$NumCompaniesWorked == x)],na.rm = T))
  })
  
  # Check Data Validity
  sapply(gen_data, function(x){levels(as.factor(x))}) # Levels are OK and within limit, No invalid data
  
# In_time and Out_time : Data Cleaning and Processing

  # NA values in these simply mean the employee didn't come to office and the biometric was not registered, hence not imputing
  
  # Coverting time to POSIXlt data for easy calculation of times  
  in_time_1 <- data.frame(sapply(in_time[,-1], function(x){ as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S")} ))
  out_time_1 <- data.frame(sapply(out_time[,-1], function(x){ as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S")} ))


#====================== DERIVE VARIABLES =========================    
  
  # Derive variable : worked_hours
  time_1 <- out_time_1 - in_time_1 # Subtracting (OutTime - Intime)
  class(time_1$X2015.01.01)

  time_df <- cbind(out_time$X,time_1) # Binding Employee ID and derived variable to time_df
  colnames(time_df)[1]  <- "EmployeeID"
  
  time_df[-1] <- lapply(time_df[-1], unclass)
  time_df[-1] <- lapply(time_df[-1], as.numeric) # Converting to numeric
  
  time_df$worked_hours_mean <- rowMeans(time_df[-1],na.rm = T) # Finding mean of worked_hours_mean

  time_df <- time_df[,c("EmployeeID","worked_hours_mean")]  
