credit <- read.csv("C:/Users/joshh/OneDrive/Documents/R Programs/Business Intelligence/credit.csv", stringsAsFactors=TRUE)

str(credit)

table(credit$checking_balance)
table(credit$credit_history)

summary(credit$months_loan_duration)
summary(credit$amount)

table(credit$default)

# Set a randomized seed for consistency in results
RNGversion("3.5.2")
set.seed(123)

train_sample <- sample(1000, 900)
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

prop.table(table(credit_train$default))
prop.table((table(credit_test$default)))

# Train a decision tree
library(C50)

# The algorithm needs a factor type if the outcome variable (something in the notes)
credit_train$default <- as.factor(credit_train$default)
credit_test$default <- as.factor(credit_test$default)
#Train a decision tree model
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model

summary(credit_model)

# create a factor vector of predictions on test data
credit_pred <- predict(credit_model,credit_test)
credit_pred

library(gmodels)
CrossTable(credit_test$default,credit_pred,
           prop.chisq=F, prop.r=F, prop.c=F,
           dnn=c('Actual Default','Predicted Default'))

# Boosted tree
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
summary(credit_boost10)

credit_boost_pred <- predict(credit_boost10,credit_test)
CrossTable(credit_test$default,credit_boost_pred,
           prop.chisq=F, prop.r=F, prop.c=F,
           dnn=c('Actual Default','Predicted Default'))

# Creating Cost Matrix
matrix_dimensions <- list(c("no", "yes"),c("no","yes"))
names(matrix_dimensions) <- c("predicted","actual")

error_cost <- matrix(c(0,1,10,0), nrow = 2, dimnames = matrix_dimensions)

# Retrain the tree with th enew penalty weights
credit_cost <- C5.0(credit_train[-17],credit_train$default,trials = 10, costs = error_cost)
summary(credit_cost)

credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default,credit_cost_pred,
           prop.chisq=F, prop.r=F, prop.c=F,
           dnn=c('Actual Default','Predicted Default'))
