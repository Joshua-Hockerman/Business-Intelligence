# Joshua Hockerman
# Decision Tree Homework

library(tidyverse)
library(C50)
library(gmodels)
library(rpart)
library(rpart.plot)

organics <- read.csv("~/R Programs/Business Intelligence/organics.csv", stringsAsFactors=TRUE)

# Q1. What is the target variable and how is it measured?

# The target variable is TargetBuy and is it measured as a binary with 1 as purchase and 0 and no purchase.

# Q2. How many customers (data records) are in the dataset?
str(organics) # There are 22223 observations in the dataset.

# Q3. How many customers have purchased organics? (Hint: use table() function)
table(organics$TargetBuy) # 5505 customers have purchased organics

# Q4. In what Television Region (i.e., DemTVReg) are most loyalty card holders
# located, how many?
table(organics$DemTVReg) # London has the most card members, with 6189 members

# Q5. What is the most common Loyalty Card Status (i.e., PromClass)?
table(organics$PromClass) # The most common status is Silver with 8572 customers

# Q6. Create a histogram for DemAge (i.e., Age)
hist(organics$DemAge)

# Q7. Show a histogram of the Age variable with bars in 1-year increments. Change
# breaks=61 option in the hist() function: i.e., (max – min) / increment = (79 – 18) / 1 =
# 61. What is the most common age range (mode)?
hist(organics$DemAge, breaks = 61) # The most common age range is 50-60 with the most common age being 51

# Q8. Is the distribution of customer age in the loyalty program right-skewed or leftskewed?
# The age distribution is left-skewed as there is a greater number of older members.

# Q9. Create a random sample for training and testing data (90% training data, 10%
# testing data) use RNGversion("3.5.2"); set.seed(123)
RNGversion("3.5.2")
set.seed(123)

train_sample <- sample(22223, 20000)
organics_train <- organics[train_sample,]
organics_test <- organics[-train_sample,]

# Q10. What are the approximate percentages of Organics purchasers and nonpurchasers in the Training and Testing data sets?
prop.table(table(organics_train$TargetBuy))
prop.table(table(organics_test$TargetBuy))

# Q11. Import package C50. Convert the target variable in both training dataset and
# testing dataset into factor variables. Then build a decision tree with C5.0() function.
organics_train$TargetBuy <- as.factor(organics_train$TargetBuy)
organics_test$TargetBuy <- as.factor(organics_test$TargetBuy)

#Train a decision tree model
organics_model <- C5.0(organics_train[-12], organics_train$TargetBuy)
organics_model

summary(organics_model)

# Q12. What variable is best at differentiating (splitting) loyalty customers
# buying organics? What is the value of the variable used to split?
# DemAge is the variable best at differentiating loyalty customers and the age is 44

# Q13. Use this decision tree to predict, whether a customer, who is a 50-year old 
# female with an Affluence grade of 15, with a Silver Loyalty class, and belongs to
# London television region, would purchase organic food.
str(organics) # This cuastomer should purchase organics food, acording to the model.

# Q14. Use the model you built to make prediction for the test dataset.
organics_pred <- predict(organics_model, organics_test)
organics_pred

# Q14. Use CrossTable() function from “gmodels” package to evaluate the
# performance of your model on test dataset. What is the percentage of customers who
# actually purchased organic food but being incorrectly predicted?
CrossTable(organics_test$TargetBuy,organics_pred,
           prop.chisq=F, prop.r=F, prop.c=F,
           dnn=c('Actual Purchase','Predicted Purchase'))
# There were 319 customers who were predicted to not purchase that did end up purchase.

# Q15. Create a boosted tree to increase performance (by specifying trials option). What is
# the lowest error rate for training data with your boosted tree model? (use summary() to examine the model outcome)
organics_boost50 <- C5.0(organics_train[-12], organics_train$TargetBuy,
                         trials = 50)
summary(organics_boost50)
# It seems like the lowest composite error rate is around 15% on this dataset and decision tree model

# Q16. Use CrossTable() function from “gmodels” package to evaluate the
# performance of your boosted model on test dataset. What is the percentage of
# customers who actually purchased organic food but being incorrectly predicted? 
organics_boost_pred <- predict(organics_boost50, organics_test)

CrossTable(organics_test$TargetBuy,organics_boost_pred,
           prop.chisq=F, prop.r=F, prop.c=F,
           dnn=c('Actual Purchase','Predicted Purchase'))
# There were 302 customers who were predicted to not purchase that did end up purchase.