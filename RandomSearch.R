# load libraries
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

set.seed(1)


setwd(".../Kaggle")

# load data
df_train = read_csv("train_users_2.csv")
df_test = read_csv("test_users.csv")
labels = df_train['country_destination']
df_train = df_train[-grep('country_destination', colnames(df_train))]

MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred))    
  ll = ll * -1/(nrow(act))      
  return(ll);
}


ndcg5 <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain,"label")
  num.class = 12
  pred <- matrix(preds, nrow = num.class)
  top <- t(apply(pred, 2, function(y) order(y)[num.class:(num.class-4)]-1))
  
  x <- ifelse(top==labels,1,0)
  dcg <- function(y) sum((2^y - 1)/log(2:(length(y)+1), base = 2))
  ndcg <- mean(apply(x,1,dcg))
  return(list(metric = "ndcg5", value = ndcg))
}


# combine train and test data
df_all = rbind(df_train,df_test)
# remove date_first_booking
df_all = df_all[-c(which(colnames(df_all) %in% c('date_first_booking')))]
# replace missing values
df_all[is.na(df_all)] <- -1

# split date_account_created in year, month and day
dac = as.data.frame(str_split_fixed(df_all$date_account_created, '-', 3))
df_all['dac_year'] = dac[,1]
df_all['dac_month'] = dac[,2]
df_all['dac_day'] = dac[,3]
df_all = df_all[,-c(which(colnames(df_all) %in% c('date_account_created')))]

# split timestamp_first_active in year, month and day
df_all['tfa_year'] = substring(as.character(df_all['timestamp_first_active']), 3, 6)
df_all['tfa_month'] = substring(as.character(df_all['timestamp_first_active']), 7, 8)
df_all['tfa_day'] = substring(as.character(df_all['timestamp_first_active']), 9, 10)
df_all = df_all[,-c(which(colnames(df_all) %in% c('timestamp_first_active')))]

# clean Age by removing values
df_all[df_all$age < 14 | df_all$age > 100,'age'] <- -1

# one-hot-encoding features
ohe_feats = c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
dummies <- dummyVars(~ gender + signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)

# split train and test
X = df_all_combined[df_all_combined$id %in% df_train$id,]
y <- recode(labels$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
X_test = df_all_combined[df_all_combined$id %in% df_test$id,]

mylogloss_list=0
myparam_list=0
run=0
run_max=5

while (run<run_max)
{
  ptm <- proc.time()
  run=run+1
  # Set necessary parameter
  
  # train xgboost
  param_xgb <- list("objective" = "multi:softprob",
                    "eval_metric" = ndcg5,
                    "num_class" = 12,
                    "eta" = runif(1,0.01,0.8),
                    "max_depth" = sample(c(3:15),1),
                    "min_child_weight" = sample(c(2:5),1),
                    "nrounds" = 5,
                    "subsample" =sample(c(5:9),1)/10,
                    "colsample_bytree" =0.8,
                    "nthread" = 2,
                    "verbose"=0) # Number of trees to fit
  clf_xgb = xgboost(param=param_xgb, 
                    data = data.matrix(X[,-1]), 
                    label = y, 
                    nrounds=param_xgb$nrounds, 
                    verbose=param_xgb$verbose
                    )
  
  
  pred_prob = predict(clf_xgb, data.matrix(X[,-1])) ## prediction on the train data
  pred_prob = matrix(pred_prob,12,length(pred_prob)/12)
  pred_prob = t(pred_prob)
  actual = matrix(0,nrow=nrow(pred_prob),ncol=ncol(pred_prob))
  for (x in 1:nrow(pred_prob)){
    actual[x,y[x]+1]=1  
  }
  mylogloss <- MultiLogLoss(actual, pred_prob)
  ptm2 <- proc.time()-ptm
  message(mylogloss, " __ calculation costs ", ptm2[1])
  
  myparam_list=rbind(myparam_list,param_xgb)
  mylogloss_list= rbind(mylogloss_list,mylogloss)
}

# predict values in test set
y_pred <- predict(clf_xgb, data.matrix(X_test[,-1]))

# extract the 5 classes with highest probabilities
predictions <- as.data.frame(matrix(y_pred, nrow=12))
rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
predictions_top5 <- as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))

# # create submission 
# ids <- NULL
# for (i in 1:NROW(X_test)) {
#   idx <- X_test$id[i]
#   ids <- append(ids, rep(idx,5))
# }
# submission <- NULL
# submission$id <- ids
# submission$country <- predictions_top5
# 
# # generate submission file
# submission <- as.data.frame(submission)
# write.csv(submission, "submission.csv", quote=FALSE, row.names = FALSE)