# Google Revenue Competition #
# By: Nick Bruno # 
library(tidyverse)
library(randomForest)
setwd('/Users/nickbruno/SYS6018/sys6018-competition-google')

##### Import datasets #####
train <- read.csv('train-flattened.csv')
names(train) # eventually groupby "fullVisitorId"

test <- read.csv('test-flattened.csv')
names(test)

##### Investigate the NA values in the response variable #####
sum(is.na(train$totals.transactionRevenue)) # 892138
nrow(train) # 903653
num_rows_with_revenue <- nrow(train) - sum(is.na(train$totals.transactionRevenue))
  # 11515 rows had revenue
# Most rows do not have a revenue

###### Organize the dataset and deal with null values #####
train$totals.transactionRevenue[is.na(train$totals.transactionRevenue)] <- 0
# train['has_revenue'] <- ifelse(train$totals.transactionRevenue == 0, 0, 1)
# train$has_revenue <- as.factor(train$has_revenue)
  # I will use this later, but for now I will drop it. Will add it later

# Drop columns that only have one unique value
n_col <- ncol(train) # 55
length_columns_unmodified <- vector() # create an empty vector
for (i in 1:n_col){
  length_columns_unmodified[i] <- length(unique(train[,i]))
} # finds the number of unique values in each row

new_train <- train
new_train[,which(length_columns_unmodified == 1)] <- NULL # deletes columns with only one unique value
ncol(new_train) # 36
  # Now there are only 36 columns

# I will do the same for the 'test' dataset
n_col_test <- ncol(test) # 3 less than the 'train' dataset
unique_values_test <- vector()
for (j in 1:n_col_test){
  unique_values_test[j] <- length(unique(test[,j])) 
}

new_test <- test
new_test[,which(unique_values_test == 1)] <- NULL
ncol(new_test) # 34
  # It seems that there are 2 columns in the 'train' dataset that are not in the 'test' dataset. I 
  # will find these and remove them from the train

setdiff(names(new_train), names(new_test))
  # [1] "totals.transactionRevenue"  "trafficSource.campaignCode"
# We know that the 'test' dataset will not have "totals.transactionRevenue", so this is not a surprise.
# However, because 'test' does not have the column "trafficSource.campaignCode", this column in the
# 'train' dataset will not help us predict 'test' transaciton revenue, so I will remove it from
# the 'train' dataset.

new_train$trafficSource.campaignCode <- NULL
ncol(new_train) # 35

# Now there are only 35 columns (one more than the 'test' dataset), so we can start the analysis.

##### Creating predictive models #####
##  IGNORE THIS FOR NOW   ##
# Create linear regression
# First issue is that most columns 
first_lm <- lm(totals.transactionRevenue ~ ., data=new_train)
summary(first_lm)
test_preds <- predict(first_lm, newdata=test)
new_test <- test
new_test$test_preds <- test_preds
group <- group_by(new_test, fullVisitorId)
nrow(summarise(group, sum=sum(test_preds)))

# Create random forest regression
rf <- randomForest(has_revenue ~ visitStartTime + totals.visits, data=train)
summary(rf)
rf_test_pred <- predict(rf, newdata=test)
df_rf <- data.frame(rf_test_pred)
df_rf$rf_test_pred <- as.numeric(df_rf$rf_test_pred)
df$new <- ifelse(df_rf$rf_test_pred == 0, 0, 1)


