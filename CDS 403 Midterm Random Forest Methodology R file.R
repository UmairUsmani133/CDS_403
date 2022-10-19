---
title: "CDS-403-001 Midterm Random Forest Project Methodology Code"
author: "Umair Usmani"
date: "10/18/2022"
output: html_document
---
  
  
install.packages("caret", dependencies = TRUE)
install.packages("randomForest")

library("caret")
library("randomForest")

covidrate_train <- read.table("C:\\Users\\Umair\\CDS 403 Project\\COVID-19_Vaccinations_in_the_United_States_County.csv", sep=",", header= TRUE)

vaccination_train <- read.table("C:\\Users\\Umair\\CDS 403 Project\\United_States_COVID-19_Community_Levels_by_County.csv", sep=",", header= TRUE)

covidrate_test <- read.table("C:\\Users\\Umair\\CDS 403 Project\\COVID-19_Vaccinations_in_the_United_States_County.csv", sep = ",", header = TRUE)

vaccination_test <- read.table("C:\\Users\\Umair\\CDS 403 Project\\United_States_COVID-19_Community_Levels_by_County.csv", sep = ",", header = TRUE)

head(covidrate_train)
head(vaccination_train)

head(covidrate_test)
head(vaccination_test)

install.packages("fields")
library(fields)
bplot.xy(train$county_fips, train$county_population)

test$county_population <- predict(model, newdata = test)

test$county_population



