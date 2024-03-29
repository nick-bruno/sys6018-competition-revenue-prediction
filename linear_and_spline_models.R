# Google Revenue Competition #

# Upload libraries
library(tidyverse)
library(randomForest)
library(car)
library(gdata)
library(splines)
setwd('C:\Users\nhb3zf\Documents\Downloads') # Will change depending on where the .csv's are located

##### Import datasets #####
train <- read.csv('train_v2_flat.csv', colClasses=c("fullVisitorId"="character"))
nrow(train) # [1] 1708337

test <- read.csv('test_v2_flat.csv', colClasses=c("fullVisitorId"="character"))
nrow(test) # [1] 401589

# These datasets have been condensed so that the only columns included are those that have more than one
# unique value and belong in both the 'train' and 'test' datasets. Also, the column "fullVisitorId" is 
# read in as a character string, which will allow us to submit the predictions later.

##### Investigate the NA values in the response variable #####
sum(is.na(train$totals_transactionRevenue)) # 1689823

num_rows_with_revenue <- nrow(train) - sum(is.na(train$totals_transactionRevenue))
  # 18514 rows have revenue
# Most rows do not have a revenue (which likely means revenue = 0)

###### Organize the dataset and deal with null values #####
train$totals_transactionRevenue[is.na(train$totals_transactionRevenue)] <- 0


##### Creating predictive models #####
# First issue is that the total.revenue for most rows = 0, so we want to find which rows of data
# the linear model predicts will be zero for the 'test' data.


##### Create logit function to predict if an observation has any revenue #####
new_train <- train

# Find which rows from the training dataset have values for revenue (greater than zero)
new_train['has_revenue'] <- ifelse(new_train$totals_transactionRevenue == 0, 0, 1)
names(new_train)

# Create logit models to predict which rows should not have revenue
logit <- glm(has_revenue ~ visitNumber + channelGrouping + geoNetwork_continent + device_deviceCategory + totals_hits + trafficSource_medium, family='binomial', data=new_train)
summary(logit)
  # channelGrouping is the only variable that is statistically insignificant

# Create predictions whether the observation should have revenue or not
logit_preds <- predict(logit, newdata=new_train)
has_revenue_pred <- ifelse(logit_preds > 0.5, 1, 0)
sum(has_revenue_pred) # 3441
  # 3441 is much smaller than the actual 18514, so I will decrease the threshold for an observation to 
  # have revenue

wider_has_revenue_pred <- ifelse(logit_preds > 0.25, 1, 0)
sum(wider_has_revenue_pred) # 3959
  # Still way too small

logit_1_has_revenue_pred <- ifelse(logit_preds > 0, 1, 0)
sum(logit_1_has_revenue_pred) # 4646
  # Stil too small, but we will leave it for now

# Add some variables to the model (and drop 'channelGrouping')
logit_2 <- glm(has_revenue ~ date + visitNumber + geoNetwork_continent + device_deviceCategory + totals_pageviews + totals_hits + trafficSource_medium, family='binomial', data=new_train)
summary(logit_2)

# Test how each model performed #
logit_2_preds <- predict(logit_2, newdata=new_train)
logit_2_preds[is.na(logit_2_preds)] <- 0
logit_2_has_revenue_pred <- ifelse(logit_2_preds > 0, 1, 0)
logit_2_has_revenue_pred[is.na(logit_2_has_revenue_pred)] <- 0
sum(logit_2_has_revenue_pred) # 5773

table_1 <- table(new_train$has_revenue, logit_1_has_revenue_pred)
# logit_1_has_revenue_pred
#       0       1
# 0 1687099    2724
# 1   16592    1922

table_2 <- table(new_train$has_revenue, logit_2_has_revenue_pred)
# logit_2_has_revenue_pred
#       0       1
# 0 1686764    3059
# 1   15800    2714

# So far, the second model seems to be a better fit. I will  try one more model to try to create more 
# accurate results.

# New model adding variables 'device_isMobile' and 'totals_timeOnSite'
logit_3 <- glm(has_revenue ~ date + visitNumber + geoNetwork_continent + device_isMobile + device_deviceCategory + totals_timeOnSite + totals_pageviews + totals_hits + trafficSource_medium, family='binomial', data=new_train)
summary(logit_3)
  # It seems that 'device_isMobile' caused multicollienarity with the 'device_deviceCategory' variable,
  # so I will remove the 'device_isMobile' variable. However, 'totals_timeOnSite' was significant, so
  # I will keep this variable in the next model.

# Final logit model (Model 4)
logit_4 <- glm(has_revenue ~ date + visitNumber + geoNetwork_continent + device_deviceCategory + totals_timeOnSite + totals_pageviews + totals_hits + trafficSource_medium, family='binomial', data=new_train)
summary(logit_4)
  # All variables are statistically significant.

# I will now test the results against the logit_2 predictions
logit_4_preds <- predict(logit_4, new_train)
logit_4_has_revenue_pred <- ifelse(logit_4_preds > 0, 1, 0)
logit_4_has_revenue_pred[is.na(logit_4_has_revenue_pred)] <- 0
sum(logit_4_has_revenue_pred) # 5229

table_4 <- table(new_train$has_revenue, logit_4_has_revenue_pred)
# logit_3_has_revenue_pred
#       0       1
# 0 1687039    2784
# 1   16069    2445

# Compare results of table_2 and table_4
table_2[2,2] / sum(table_2[,2]) # 0.4701195
table_4[2,2] / sum(table_4[,2]) # 0.4675846
  # It seems that logit_2 is actually slightly better at predicting if a row should have revenue, so I 
  # will use that for the OLS model. 

# Create a wider threshold to account for more observations that have a revenue
# We want the number to be close to 18514
logit_2_wider_preds <- ifelse(logit_2_preds > -0.25, 1, 0)
logit_2_has_revenue_pred_wider <- ifelse(logit_2_wider_preds > 0, 1, 0)
logit_2_has_revenue_pred_wider[is.na(logit_2_has_revenue_pred_wider)] <- 0
sum(logit_2_has_revenue_pred_wider) # 6994
  # Still not accurate enough

logit_2_wider_preds_2 <- ifelse(logit_2_preds > -1, 1, 0)
logit_2_has_revenue_pred_wider_2 <- ifelse(logit_2_wider_preds_2 > 0, 1, 0)
logit_2_has_revenue_pred_wider_2[is.na(logit_2_has_revenue_pred_wider_2)] <- 0
sum(logit_2_has_revenue_pred_wider_2) # 11366

logit_2_wider_preds_3 <- ifelse(logit_2_preds > -2, 1, 0)
logit_2_has_revenue_pred_wider_3 <- ifelse(logit_2_wider_preds_3 > 0, 1, 0)
logit_2_has_revenue_pred_wider_3[is.na(logit_2_has_revenue_pred_wider_3)] <- 0
sum(logit_2_has_revenue_pred_wider_3) # 23654 (a little too big)

logit_2_wider_preds_4 <- ifelse(logit_2_preds > -1.75, 1, 0)
logit_2_has_revenue_pred_wider_4 <- ifelse(logit_2_wider_preds_4 > 0, 1, 0)
logit_2_has_revenue_pred_wider_4[is.na(logit_2_has_revenue_pred_wider_4)] <- 0
sum(logit_2_has_revenue_pred_wider_4) # 19535 (more accurate)

# We have determined that moving the threshold to -1.75 gives us a wider amount of predictions
# where expected observations have revenue.
final_table <- table(new_train$has_revenue, logit_2_has_revenue_pred_wider_4)
# logit_2_has_revenue_pred_wider_4
#       0       1
# 0 1677900   11923
# 1   10902    7612

final_table[2,2] / sum(final_table[,2]) # 0.3896596
  # A little less accurate, but much more encompassing

##### Create linear regression #####
# I only want to use the rows that have revenue
obs_have_revenue <- which(new_train$has_revenue > 0)
length(obs_have_revenue)

# I will use cross validation, so I will split the training data 
n <- length(obs_have_revenue)
set.seed(1)
samp <- sample(n, round(n*0.75))
train_has_rev <- new_train[obs_have_revenue, ]

train_valid <- train_has_rev[samp,] # will use this to create my linear models
test_valid <- train_has_rev[-samp,]

# Create first linear model using the same variables from the 'logit_2' function
names(train)
lm_1 <- lm(log(totals_transactionRevenue) ~ date + totals_timeOnSite + visitNumber + geoNetwork_continent + device_deviceCategory + totals_pageviews + totals_hits + trafficSource_medium, data=train_valid)
summary(lm_1)
  # all significant except date and visitNumber

# Second model will use the same variables from 'logit_4' function
lm_2 <- lm(log(totals_transactionRevenue) ~ date + visitNumber + geoNetwork_continent + device_deviceCategory + totals_pageviews + totals_hits + trafficSource_medium, data=train_valid)
summary(lm_2)
  # same as above (date is not significant, trafficSource_medium barely significant)

# Third linear model
lm_3 <- lm(log(totals_transactionRevenue) ~  visitNumber + geoNetwork_continent + device_deviceCategory + totals_pageviews + totals_hits + trafficSource_medium, data=train_valid)
summary(lm_3)
  # all are statistically significant

# Test the results using cross validation
lm_1_preds <- predict(lm_1, test_valid)
lm_1_preds[is.na(lm_1_preds)] <- 0
lm_2_preds <- predict(lm_2, test_valid)
lm_2_preds[is.na(lm_2_preds)] <- 0
lm_3_preds <- predict(lm_3, test_valid)
lm_3_preds[is.na(lm_3_preds)] <- 0


test_valid['log_rev'] <- log(test_valid$totals_transactionRevenue)
mse_1 <- sum((lm_1_preds - test_valid$log_rev)^2)
  # 6137.851
mse_2 <- sum((lm_2_preds - test_valid$log_rev)^2)
  # 5807.155
mse_3 <- sum((lm_3_preds - test_valid$log_rev)^2)
  # 5806.419

# The third linear model seems to be the best model, so I will use that (although it is very close to
# the second model).


##### Apply logit and regression model to the 'test' dataset #####
test_logit_preds <- predict(logit_2, test)
test_has_revenue_preds <- ifelse(test_logit_preds > -1.75, 1, 0)
test_has_revenue_preds[is.na(test_has_revenue_preds)] <- 0
sum(test_has_revenue_preds) # 4753

test_predict_zero_revenue_row_numbers <- which(test_has_revenue_preds == 0)
  # finds the rows that we predict to have no revenue

# Compare proportion of rows that are predicted to have no revenue with the 'train' data
prop_test_row_has_revenue <- sum(test_has_revenue_preds)/nrow(test) # 0.01183548
prop_train_row_has_revenue <- sum(new_train$has_revenue)/nrow(new_train) #  0.01083744
  # We predict that there are slightly more rows in the 'test' data that should have revenue

new_test <- test # keeping the integrity of 'test'
test_zero_rev_predictions <- new_test[test_predict_zero_revenue_row_numbers,]
test_has_rev_predictions <- new_test[-test_predict_zero_revenue_row_numbers,]

# Now we will predict the revenue for those rows we think should have revenue using our linear regression
test_linear_preds <- data.frame(predict(lm_3, test_has_rev_predictions))

# Combine the zero predictions with the revenue predictions
n_zero <- length(test_predict_zero_revenue_row_numbers) # 396836
test_no_rev <- data.frame(test_predict_zero_revenue_row_numbers, rep(0,n_zero))
test_no_rev$test_predict_zero_revenue_row_numbers <- NULL

names(test_no_rev) <- "Prediction"
names(test_linear_preds) <- "Prediction"
full_linear_pred_rows <- rbind(test_no_rev, test_linear_preds)
  # Combines all of the linear predictions (Those with and those without revenue)

new_test['Prediction'] <- full_linear_pred_rows$Prediction

# Need to group the results by 'fullVisitorId'
df_test_linear_preds <- select(new_test, fullVisitorId, Prediction)
lin_gb <- group_by(df_test_linear_preds, fullVisitorId)
all_lin_preds_new <- summarise(lin_gb, sum=sum(Prediction))
nrow(all_lin_preds_new) # 296530

# Now that we have the predictions, we must order them to match the kaggle csv
submission <- read.csv('sample_submission_v2.csv')
names(submission)
nrow(submission) #  296530 (matches my predictions)

names(all_lin_preds_new) <- c('fullVisitorId', 'PredictedLogRevenue')
  # Matches the names of the csv we will write out to the kaggle submissions

##### Write the linear results to the .csv #####
write.csv(all_lin_preds_new, row.names=F, file = "Corrected_Linear_Predictions.csv")
  # We are now done with the linear predictions and will move on to spline predictions

##### Spline Based Regression #####
# Use the same cross-validation method 
# Take our best linear model and apply different splines to it
spline_1 <- lm(log(totals_transactionRevenue) ~  visitNumber + geoNetwork_continent + device_deviceCategory + ns(totals_pageviews, df=6) + ns(totals_hits, df=6) + trafficSource_medium, data=train_valid)
summary(spline_1)

spline_2 <- lm(log(totals_transactionRevenue) ~  ns(visitNumber, df=6) + geoNetwork_continent + device_deviceCategory + ns(totals_pageviews, df=4) + ns(totals_hits, df=4) + trafficSource_medium, data=train_valid)
summary(spline_2)

spline_3 <- lm(log(totals_transactionRevenue) ~  ns(visitNumber, df=10) + geoNetwork_continent + device_deviceCategory + ns(totals_pageviews, df=10) + ns(totals_hits, df=10) + trafficSource_medium, data=train_valid)
summary(spline_3)

# Create predictions from the models
spline_pred_1 <- predict(spline_1, test_valid)
spline_pred_2 <- predict(spline_2, test_valid)
spline_pred_3 <- predict(spline_3, test_valid)

mse_spline_1 <- sum((spline_pred_1 - test_valid$log_rev)^2)
  #  5679.262
mse_spline_2 <- sum((spline_pred_2 - test_valid$log_rev)^2)
  # 5545.877
mse_spline_3 <- sum((spline_pred_3 - test_valid$log_rev)^2)
  # 5531.965

# Spline 3 seems to be the most successful (also the most complex), so we will use this for the predictions.

##### Apply spline_3 function to 'test' dataset that we predict will have revenue #####
test_spline_preds <- data.frame(predict(spline_3, test_has_rev_predictions))
names(test_no_rev) <- "Prediction"
names(test_spline_preds) <- "Prediction"
all_spline_pred_rows <- rbind(test_no_rev, test_spline_preds)

new_test['Prediction'] <- all_spline_pred_rows$Prediction

# Groupby 'fullVisitorId' to comply with Kaggle competition
df_test_spline_preds <- select(new_test, fullVisitorId, Prediction)
spline_gb <- group_by(df_test_spline_preds, fullVisitorId)
all_spline_preds <- summarise(spline_gb, sum=sum(Prediction))
nrow(all_spline_preds) # 296530

names(all_spline_preds) <- c('fullVisitorId', 'PredictedLogRevenue')
  # Column names now match with Kaggle requirements.

##### Write to csv #####
write.csv(all_spline_preds, row.names=F, file='Spline_Predictions.csv')
