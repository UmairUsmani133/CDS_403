---
title: "CDS 403 Final"
author: "Umair Usmani"
date: "12/04/2022"
output: html_document
---
  
#This is the R Script Code File
#This code includes all the datasets used regarding the CDC's collection of Covid-19 Data collected
#This code also includes manually created data which is then also manually merged with the CDC Data for ML use
#The weather data is manually collected from "https://accuweather.com"
#For the purposes of being able to easily run the code, a specific County (Van Buren County) was chosen to use the code on,
#if desired by the user, more data can be manually collected for different counties for more testing


#These files can be downloaded by exporting them through the links below
  
covid_rates<-read.csv("C:\\Users\\Umair\\CDS 403 Project\\COVID-19_Vaccinations_in_the_United_States_County.csv") 

#https://covid.cdc.gov/covid-data-tracker/#county-view?list_select_state=all_states&list_select_county=all_counties&data-type=Risk

vaccinations<-read.csv("C:\\Users\\Umair\\CDS 403 Project\\United_States_COVID-19_Community_Levels_by_County.csv")

#https://data.cdc.gov/Public-Health-Surveillance/United-States-COVID-19-Community-Levels-by-County/3nnm-4jni

#summary of CDC covid rates data
summary(covid_rates)


###The presentation of this project which will be turned in with this code, will also be on the github repository linked
###for general information, results, and additional baseline models to better understand the project
###https://github.com/UmairUsmani133/CDS_403/blob/main/CDS%20403%20Midterm%20R%20file.R

###The purpose of this project is given in the presentation as well, regarding how no one has continued the study 
###how weather effects the covid-19 spread after the initial claims 


#downloading library
library(ggplot2)


#Loading in the manual data created in excel csv sheets (all can be found in the github repository)
buren_fips<-read.csv("C:\\Users\\Umair\\CDS 403 Project\\Van Buren County, TN, FIPS_47175 - Sheet1.csv")
avg_weather<-read.csv("C:\\Users\\Umair\\CDS 403 Project\\VanBurenCounty_AvgWeather_FinalSheet.csv")
louden_weather<-read.csv("C:\\Users\\Umair\\CDS 403 Project\\LoudonCountyVAWeatherDatabyZipCodes.xlsx - Sheet1.csv")



#The charts below show some of the baseline charts for each of the data sources in addition to 
#baseline data of covid-19 cases that can be reffered to found on google (as seen in powerpoint presentation)
ggplot(data=buren_fips, aes(x=MonthEndDate, y=covid_cases_per_100k, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=avg_weather, aes(x=Month_Name, y=AvgTemp, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=avg_weather, aes(x=AvgTemp, y=CovidCases, group=1)) +
  geom_line()+
  geom_point()


#______________________________________________________________________________________________________________

#Below is the first ML method used (KNN), including the confusion matrix

###The reasoning for why this method was used for this project is given in the presentation (Slides 7/8)

#installing and loading up the required libraries
install.packages("e1071")
install.packages("caTools")
install.packages("class")

library(e1071)
library(caTools)
library(class)

#Splitting data into train and test data
split <- sample.split(avg_weather, SplitRatio = 0.7)
train_cl <- subset(avg_weather, split == "TRUE")
test_cl <- subset(avg_weather, split == "FALSE")


# Feature Scaling
train_scale <- scale(train_cl[, 3:6])
test_scale <- scale(test_cl[, 3:6])

# Fitting KNN Model to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$AvgTemp,
                      k = 1)
classifier_knn




# Confusion Matrix
cm <- table(test_cl$CovidCases, classifier_knn)
cm


# Model Evaluation - Choosing K, more levels of K can be added as well
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$AvgTemp)
print(paste('Accuracy =', 1-misClassError))



#Below are the same steps being taken above but this time to be used to make a confusion matrix
#in order to find the predicted values

split <- sample.split(avg_weather, SplitRatio = 0.7)
train_cl <- subset(avg_weather, split == "TRUE")
test_cl <- subset(avg_weather, split == "FALSE")



train_scale <- scale(train_cl[, 5:6])
test_scale <- scale(test_cl[, 5:6])


set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(AvgTemp ~ ., data = train_cl)
classifier_cl



y_pred <- predict(classifier_cl, newdata = test_cl)


cm <- table(test_cl$AvgTemp, y_pred)
cm


confusionMatrix(cm)


#______________________________________________________________________________________________________________

#Below is the Decision tree method being used (Similar to the KNN above, reasoning is given in the Presentation slide 7/8)

avgweather_covidrates <- read.csv("C:\\Users\\Umair\\CDS 403 Project\\VanBurenCounty_AvgWeather - Sheet1.csv")
str(avgweather_covidrates) #converting data to string

#generating table for two variables
table(avgweather_covidrates$AvgTemp)
table(avgweather_covidrates$CovidCases)

#summary of the two variables
summary(avgweather_covidrates$AvgTemp)
summary(avgweather_covidrates$CovidCases)

#Using the added "Limit" variable (Explained in presentation slide 5)
table(avgweather_covidrates$Limit)

#Training the Data
set.seed(3437)
train_sample <- sample(61, 1)
str(train_sample)

#Testing the data
avgweather_covidrates_train <- avgweather_covidrates[train_sample, ]
avgweather_covidrates_test  <- avgweather_covidrates[-train_sample, ]
prop.table(table(avgweather_covidrates_train$Limit))
prop.table(table(avgweather_covidrates_test$Limit))

#Showing results of predicted series of given avg weather and the predicted covid cases
#It will be seen compared to the base model the middle avg temperatures seem to correlate more, 
#mostly due to the usage of avg weather in the data rather than individual data by day within a month
library(C50)
avgweather_covidrates <- C5.0(avgweather_covidrates_train[-1], as.factor(avgweather_covidrates_train$Limit))
avgweather_covidrates
summary(avgweather_covidrates)


#______________________________________________________________________________________________________________

# Below is an added piece of code which was reccomended in order to identify which ML method would be the most accurate
# for this project

### While this is a recent addition and was not used for this project as the reasoning for using KNN and decision 
### trees was already established, this code can be used later for other purposes if more variables are added

#installing packages
install.packages("caret")

install.packages("caret", dependencies=c("Depends", "Suggests"))

library(caret)

dataset <- avg_weather


colnames(dataset)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$CovidCases, p=0.80, list=FALSE)
dataset <- dataset[validation_index,]
dim(dataset)


sapply(dataset, class)


head(dataset)


#summarize the class distribution
percentage <- prop.table(table(dataset$CovidCases)) * 100

# summarize attribute distributions
summary(dataset)


#This code below isn't meant to be used with the current datasets, however, I believe it is essential to 
#use this code (given the proper data), in order to evaluate 5 big ML algorithms and their accuracies given the data.
#This can help choose which ML algorithm may be the best choice to run on certain datasets. 

# a) linear algorithms
set.seed(7)
fit.lda <- train(CovidCases~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(CovidCases~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(CovidCases~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(CovidCases~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(CovidCases~., data=dataset, method="rf", metric=metric, trControl=control)
```

















