# Set a randomized seed for consistency in results
RNGversion("3.5.2")
set.seed(123)

library(C50)
library(gmodels)
library(rpart)
library(rpart.plot)

wine <- read.csv("C:/Users/joshh/OneDrive/Documents/R Programs/Business Intelligence/winequality-white.csv", stringsAsFactors=TRUE)

str(wine)

hist(wine$quality)
summary(wine)

wine_train <- wine[1:3750,]
wine_test <- wine[3751:4898,]

m.rpart <- rpart(quality ~ ., data = wine_train)
summary(m.rpart)
rpart.plot(m.rpart,digits = 3)

# Function MAE
MAE <- function(actual, predicted){
  mean(abs(actual~predicted))
}

MAE(wine_test$quality,p.)