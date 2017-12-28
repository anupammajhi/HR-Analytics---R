
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
