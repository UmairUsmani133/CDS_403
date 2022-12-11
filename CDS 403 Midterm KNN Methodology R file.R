---
title: "CDS-403-001 Midterm Project Methodology Code"
author: "Umair Usmani"
date: "10/18/2022"
output: html_document
---


#These files can be downloaded by exporting them through the links below
  
covid_rates<-read.csv("C:\\Users\\Umair\\CDS 403 Project\\COVID-19_Vaccinations_in_the_United_States_County.csv") 

#https://covid.cdc.gov/covid-data-tracker/#county-view?list_select_state=all_states&list_select_county=all_counties&data-type=Risk

vaccinations<-read.csv("C:\\Users\\Umair\\CDS 403 Project\\United_States_COVID-19_Community_Levels_by_County.csv")

#https://data.cdc.gov/Public-Health-Surveillance/United-States-COVID-19-Community-Levels-by-County/3nnm-4jni

summary(covid_rates)



summary(vaccinations)

#Future integration of weather will be extracted from the database link below:

#https://www.weather.gov/dtx/fnttemp1920-1940


# Loading package
library(e1071)
library(caTools)
library(class)

### This function also needs to be rerun for the covid_rates dataset using the covid.19_community_level variable

# Splitting data into train
# and test data
split <- sample.split(vaccinations, SplitRatio = 0.7)
train_cl <- subset(vaccinations, split == "TRUE")
test_cl <- subset(vaccinations, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

### Still need to remove Blank values in dataset for Classifier to be complete

# Fitting KNN Model 
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Completeness_pct,
                      k = 1)
classifier_knn

# Confusion Matrix
cm <- table(test_cl$Completeness_pct, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Completeness_pct)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Completeness_pct,
                      k = 3)
misClassError <- mean(classifier_knn != test_cl$Completeness_pct)
print(paste('Accuracy =', 1-misClassError))

# K = 5
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Completeness_pct,
                      k = 5)
misClassError <- mean(classifier_knn != test_cl$Completeness_pct)
print(paste('Accuracy =', 1-misClassError))

# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Completeness_pct,
                      k = 7)
misClassError <- mean(classifier_knn != test_cl$Completeness_pct)
print(paste('Accuracy =', 1-misClassError))

# K = 15
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Completeness_pct,
                      k = 15)
misClassError <- mean(classifier_knn != test_cl$Completeness_pct)
print(paste('Accuracy =', 1-misClassError))

# K = 19
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Completeness_pct,
                      k = 19)
misClassError <- mean(classifier_knn != test_cl$Completeness_pct)
print(paste('Accuracy =', 1-misClassError))
