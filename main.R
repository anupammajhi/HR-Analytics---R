
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
