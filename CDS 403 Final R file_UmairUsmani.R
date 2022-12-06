---
title: "CDS 403 Final"
author: "Umair Usmani"
date: "12/04/2022"
output: html_document
---
  
#This is the R Script Code  

  
```{r}
library(ggplot2)
```

```{r setup, include=FALSE}
buren_fips<-read.csv("C:\\Users\\Umair\\CDS 403 Project\\Van Buren County, TN, FIPS_47175 - Sheet1.csv")
```

```{r setup, include=FALSE}
avg_weather<-read.csv("C:\\Users\\Umair\\CDS 403 Project\\VanBurenCounty_AvgWeather - Sheet1.csv")
```

```{r setup, include=FALSE}
louden_weather<-read.csv("C:\\Users\\Umair\\CDS 403 Project\\LoudonCountyVAWeatherDatabyZipCodes.xlsx - Sheet1.csv")
```

```{r}
ggplot(data=buren_fips, aes(x=MonthEndDate, y=covid_cases_per_100k, group=1)) +
  geom_line()+
  geom_point()
```

```{r}
ggplot(data=avg_weather, aes(x=Month_Name, y=AvgTemp, group=1)) +
  geom_line()+
  geom_point()
```

```{r}
ggplot(data=avg_weather, aes(x=AvgTemp, y=CovidCases, group=1)) +
  geom_line()+
  geom_point()
```


#______________________________________________________________________________________________________________

```{r}
install.packages("e1071")
install.packages("caTools")
install.packages("class")
```

```{r}
library(e1071)
library(caTools)
library(class)
```

```{r}
#Splitting data into train and test data
split <- sample.split(avg_weather, SplitRatio = 0.7)
train_cl <- subset(avg_weather, split == "TRUE")
test_cl <- subset(avg_weather, split == "FALSE")
```

```{r}
# Feature Scaling
train_scale <- scale(train_cl[, 3:6])
test_scale <- scale(test_cl[, 3:6])
```

```{r}
# Fitting KNN Model to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$AvgTemp,
                      k = 1)
classifier_knn
```



```{r}
# Confusiin Matrix
cm <- table(test_cl$CovidCases, classifier_knn)
cm
```

```{r}
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$AvgTemp)
print(paste('Accuracy =', 1-misClassError))
```



```{r}
install.packages("e1071")
install.packages("caTools")
install.packages("caret")
```


```{r}
library(e1071)
library(caTools)
library(caret)
```


```{r}
split <- sample.split(avg_weather, SplitRatio = 0.7)
train_cl <- subset(avg_weather, split == "TRUE")
test_cl <- subset(avg_weather, split == "FALSE")
```

```{r}
train_scale <- scale(train_cl[, 5:6])
test_scale <- scale(test_cl[, 5:6])
```

```{r}
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(AvgTemp ~ ., data = train_cl)
classifier_cl
```

```{r}
y_pred <- predict(classifier_cl, newdata = test_cl)
```

```{r}
cm <- table(test_cl$AvgTemp, y_pred)
cm
```

```{r}
confusionMatrix(cm)
```

#______________________________________________________________________________________________________________

credit <- read.csv("C:\\Users\\Umair\\CDS 403 Project\\VanBurenCounty_AvgWeather - Sheet1.csv")
str(credit)

table(credit$AvgTemp)
table(credit$CovidCases)

summary(credit$AvgTemp)
summary(credit$CovidCases)

table(credit$Limit)


set.seed(3437)
train_sample <- sample(61, 1)
str(train_sample)

credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]
prop.table(table(credit_train$Limit))
prop.table(table(credit_test$Limit))

library(C50)
credit_model <- C5.0(credit_train[-1], as.factor(credit_train$Limit))
credit_model
summary(credit_model)

# The numbers in parentheses indicate the number of examples meeting the criteria for that decision, and the number incorrectly classified by the decision. 
# Contradictory rules occur sometimes. They might reflect a real pattern in the data, or they may be a statistical anomaly.
# Decision trees are known for having a tendency to overfit the model to the training data. For this reason, the error rate reported on training data may be overly optimistic, 
# and it is especially important to evaluate decision trees on a test dataset.

#______________________________________________________________________________________________________________

```{r}
install.packages("caret")

install.packages("caret", dependencies=c("Depends", "Suggests"))

library(caret)
```

```{r}
dataset <- avg_weather
```


```{r}
colnames(dataset)
```

```{r}
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$CovidCases, p=0.80, list=FALSE)
dataset <- dataset[validation_index,]
dim(dataset)
```

```{r}
sapply(dataset, class)
```

```{r}
head(dataset)
```

```{r}
#summarize the class distribution
percentage <- prop.table(table(dataset$CovidCases)) * 100
```

```{r}
# summarize attribute distributions
summary(dataset)
```

```{r}

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

















