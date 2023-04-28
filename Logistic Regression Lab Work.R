library(titanic)

titanic_train <- read.csv("~/R Programs/Business Intelligence/Titanic_train.csv", stringsAsFactors=FALSE)
titanic_test <- read.csv("~/R Programs/Business Intelligence/Titanic_test.csv", stringsAsFactors=FALSE)

titanic_test$Survived <- NA

complete_data <- rbind(titanic_train, titanic_test)

str(complete_data)

colSums(is.na(complete_data))

colSums(complete_data == "")

complete_data$Embarked[complete_data$Embarked==''] <- 'S'
complete_data$Age[is.na(complete_data$Age)] <- median(complete_data$Age, na.rm = T)

sapply(complete_data, function(x) length(unique(x)))

library(dplyr)

titanic_data <- complete_data %>% select(-c(Cabin,PassengerId,Name,Ticket))

for (i in c("Survived", "Pclass", "Sex", "Embarked")){
  titanic_data[,i]=as.factor(titanic_data[,i])
}

train_data <- titanic_data[1:667,]
test_data <- titanic_data[668:889,]

titanic_model_1 <- glm(Survived~.,family=binomial(link="logit"),train_data)

summary(titanic_model_1)
anova(titanic_model_1, test="Chisq")

results <- predict(titanic_model_1,test_data,type="response")
results <- ifelse(results>0.5,1,0)
results <- as.factor(results)

test_data$Survived <- as.factor(test_data$Survived)

library(caret)
confusionMatrix(results, reference = test_data$Survived)
