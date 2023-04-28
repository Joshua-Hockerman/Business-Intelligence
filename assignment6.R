# Joshua Hockerman
# MIS 5320
# Assignment 6

library(dplyr)
library(caret)

organics <- read.csv("~/R Programs/Business Intelligence/organics.csv", stringsAsFactors=FALSE)

# 1.	What variables include missing data (NAs)? Which input variable has the most missing data in the dataset?

colSums(is.na(organics)) # DemAffl, DemAge, and PromTime has NAs, and DemAge has the most NAs

# 2.	Impute missing numeric values with their mean values. 

organics$DemAffl[is.na(organics$DemAffl)] <- mean(organics$DemAffl, na.rm = T)
organics$DemAge[is.na(organics$DemAge)] <- mean(organics$DemAge, na.rm = T)
organics$PromTime[is.na(organics$PromTime)] <- mean(organics$PromTime, na.rm = T)

#B.	Assessment of Skewness
# 3.	Create a boxplot for Affluence grade (DemAff). What is the skewness of Affluence grade?
boxplot(organics$DemAffl)# The data is right-skewed as the median is close to 9 but there are data points all the way up to 34.

#  4.	Create an additional logged variable log_Affluence [Hint: log(x+1)] in the organic dataset to transform Affluence grade
organics$log_affluence <- log(organics$DemAffl + 1)
  
# 5.	What is the skewness of Affluence grade after log transformation?
boxplot(organics$log_affluence) # The distribution is mostly evenly distributed in the log dataset

#  6.	Create logged variables for all the interval input variables. (Reference the table above). 
organics$log_age <- log(organics$DemAge + 1)
organics$log_spend <- log(organics$PromSpend + 1)
organics$log_time <- log(organics$PromTime + 1)

#7.	Remove the original interval input variables, and save the rest as a new dataframe. 
organics_data <- organics %>% select(-c(DemAffl,DemAge,PromSpend,PromTime))

#8.	Split the data into training and testing datasets. Use the first 20000 observations for training and the rest for testing data. 
organics_train <- organics_data[1:20000,]
organics_test <- organics_data[20001:22223,]

# C.	Logistic Regressions
# 9.	Create a logistic model with the training dataset. Summarize the resulted model. 
organics_model_one <- glm(TargetBuy~.,family=binomial(link="logit"),organics_train)
summary(organics_model_one)

# 10.	Conduct an ANOVA analysis. What features between group means are statistically significant? 
anova(organics_model_one,test="Chisq") # The features with a "Three star" significance are DemClusterGroup,
# DemGender, PromClass, lof_affluence, and log_age. Other features that could be statistically significant,
# depending on the chosen p-value, are DemReg and DemTVReg.
  
#  11.	Predict with the Logistic model you build with the test data. 
results <- predict(organics_model_one,organics_test,type="response")
summary(results)
results <- ifelse(results > 0.5, 1, 0)

results <- as.factor(results)
organics_test$TargetBuy <- as.factor(organics_test$TargetBuy)

# 12.	Create a confusion Matrix. What is your accuracy rate? 
confusionMatrix(data=results,reference = organics_test$TargetBuy) #The accuracy of this model is 80.07%
# Most of the errors come from the model predicting a person will not purchase when they will actually purchase.
  