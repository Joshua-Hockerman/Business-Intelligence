#install.packages("titanic")
library(titanic)

## Load the datasets
titanic_train <- read.csv("~/R Programs/Business Intelligence/Titanic_train.csv", stringsAsFactors=FALSE)
titanic_test <- read.csv("~/R Programs/Business Intelligence/Titanic_test.csv", stringsAsFactors=FALSE)

## Setting Survived column for test data to NA
titanic_test$Survived <- NA

## Combining Training and Testing dataset
complete_data <- rbind(titanic_train, titanic_test)

## Check data structure
str(complete_data)

## Let's check for any missing values in the data
colSums(is.na(complete_data))

## Checking for empty values
colSums(complete_data=='')

## Check number of unique values for each of the column to find out columns which we can convert to factors
sapply(complete_data, function(x) length(unique(x)))

## Removing Cabin as it has very high missing values, passengerId, Ticket and Name are not required
library(dplyr)
titanic_data <- complete_data %>% select(-c(Cabin, PassengerId, Ticket, Name))

## Converting "Survived","Pclass","Sex","Embarked" to factors
for (i in c("Survived","Pclass","Sex","Embarked")){
  titanic_data[,i]=as.factor(titanic_data[,i])
}

## Missing values imputation
complete_data$Embarked[complete_data$Embarked==""] <- "S"

complete_data$Age[is.na(complete_data$Age)] <- median(complete_data$Age,na.rm=T)

#Split into training and testing dataset
train <- titanic_data[1:667,]
test <- titanic_data[668:889,]

## Model Creation
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
## Model Summary
summary(model)

## Using anova() to analyze the table of deviance
anova(model, test="Chisq")

## Predicting Test Data
result <- predict(model,newdata=test,type='response')
result <- ifelse(result > 0.5,1,0)

result<-as.factor(result)
test$Survived<- as.factor(test$Survived)

## Confusion matrix and statistics
#install.packages("caret")
library(caret)
confusionMatrix(data=result, reference=test$Survived)
