library(tidyverse)
library(ISLR2)
library(ggplot2)
library(dplyr)
library(GGally)
library(corrplot)
library(superml)
library(Metrics)
library(knitr)

df <- read.csv("garments_worker_productivity.csv")
names(df)
glimpse(df)
summary(df)#For the analysis of data, displays mean, median, etc
df = subset(df, select = -c(date, day) )#Removing date and day
dim(df)
is.null(df)

lapply(df,function(x) { length(which(is.na(x)))})
#By checking the wip column has 506 null values
df <- df %>% replace(is.na(.), 0)#The null values are replaced with zero
lapply(df,function(x) { length(which(is.na(x)))})#Checked again, no null values
#Detecting the outliers
#Detected using box plots


i_Q1=quantile(df$incentive, probs = c(.25))
i_Q3=quantile(df$incentive, probs = c(.75))
i_IQR=i_Q3-i_Q1
i_lower = i_Q1 - 1.5*i_IQR
i_upper = i_Q3 + 1.5*i_IQR
df1<-df[df$incentive >i_lower & df$incentive <i_upper, ]
dim(df1)
wip_Q1=quantile(df1$wip, probs = c(.25))
wip_Q3=quantile(df1$wip, probs = c(.75))
wip_IQR=wip_Q3-wip_Q1
wip_lower = wip_Q1 - 1.5*wip_IQR
wip_upper = wip_Q3 + 1.5*wip_IQR
wip_lower
df2<-df1[df1$wip >wip_lower & df1$wip<wip_upper, ]
dim(df2)
ot_Q1=quantile(df2$over_time, probs = c(.25))
ot_Q3=quantile(df2$over_time, probs = c(.75))
ot_IQR=ot_Q3-ot_Q1
ot_lower = ot_Q1 - 1.5*ot_IQR
ot_upper = ot_Q3 + 1.5*ot_IQR
ot_lower
f_df<-df2[df2$over_time >ot_lower & df2$over_time<ot_upper, ]
dim(f_df)
dim(df)
#So after updating nan's and removing outliers the dataset dimension is 1176*13
#Label encoding 
f_df$quarter <- as.numeric(factor(f_df$quarter))
head(f_df)
f_df$department[f_df$department == "finishing " | 
                  f_df$department == "finishing"] <- 1
f_df$department[f_df$department == "sweing" ] <- 2
head(f_df)
tail(f_df)
f_df$department <- as.numeric(factor(f_df$department))
M = cor(f_df)
corrplot(M)
summary(f_df)

set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(f_df), replace=TRUE, prob=c(0.7,0.3))
train  <- f_df[sample, ]
test   <- f_df[!sample, ]
dim(train)
dim(test)
drop <- c("actual_productivity")
x_train = train[,!(names(train) %in% drop)]
x_test =test[,!(names(test) %in% drop)]
y_train = train[,(names(train) %in% drop)]
y_test = test[,(names(test) %in% drop)]
dim(y_train)
x_s_train<- scale(x_train)
x_s_test<-scale(x_test)
actual_productivity<-c(y_train)
train_xy <- cbind(x_s_train, actual_productivity)
head(train_xy)
dim(train_xy)
typeof(train_xy)
train_xy <- as.data.frame(train_xy)
x_s_test <- as.data.frame(x_s_test)
#Now the scaled training and testing data are ready
#Linear Regression:
lr <- lm(actual_productivity ~ .,data=train_xy)
summary(lr)
lr_pred <- predict(lr,x_s_test)
#mse
lr_mse<-mean((y_test - lr_pred)^2)
#rmse
lr_rmse<-mean((y_test - lr_pred)^2)^(1/2)
#mae
lr_mae<-mae(y_test, lr_pred)
plot(lr)

lr_mae

#PCR:
library(pls) 
pcr_mse_tot=c()
pcr_rmse_tot=c()
pcr_mae_tot=c()
for (x in 1:11) {
  fit_pcr <- pcr(actual_productivity ~ .,ncomp = x,data=train_xy)
  summary(fit_pcr)
  pcr_pred <- predict(fit_pcr,x_s_test)
  #mse
  pcr_mse<-mean((y_test - pcr_pred)^2)
  print(pcr_mse)
  pcr_mse_tot <- append(pcr_mse_tot,pcr_mse)
  #rmse
  pcr_rmse<-mean((y_test - pcr_pred)^2)^(1/2)
  print(pcr_rmse)
  pcr_rmse_tot <- append(pcr_rmse_tot,pcr_rmse)
  #mae
  pcr_mae<-mae(y_test, pcr_pred)
  print(pcr_mae)
  pcr_mae_tot <- append(pcr_mae_tot,pcr_mae)
}
pcr_i=which.min(pcr_mae_tot)
pcr_mae_tot[pcr_i]
cat('The errors for PCR is minimum at n =',pcr_i,'components.')
#Ridge Regression:
library(glmnet)
library(glmnetUtils)
set.seed(42) # set seed for cross validation
#glmnet uses cross validation to select the optimal lambda values
cv_ridge <- cv.glmnet(actual_productivity ~ ., data = train_xy, alpha = 0)  
lambda_select_ridge <- cv_ridge$lambda.1se  # tuned lambda
lambda_select_ridge
fit_ridge_select <- glmnet(
  actual_productivity ~ ., data = train_xy,
  alpha = 0, 
  lambda = lambda_select_ridge
)
summary(fit_ridge_select)
fit_ridge_select
ridge_pred <- predict(fit_ridge_select,x_s_test)
summary(ridge_pred)
#mse
ridge_mse<-mean((y_test - ridge_pred)^2)
#rmse
ridge_rmse<-mean((y_test - ridge_pred)^2)^(1/2)
#mae
ridge_mae<-mae(y_test, ridge_pred)
#Lasso Regression
cv_lasso <-  cv.glmnet(actual_productivity ~ ., data = train_xy, alpha = 1)
lambda_select_lasso <- cv_lasso$lambda.1se  # tuned lambda
lambda_select_lasso
fit_lasso_select <- glmnet(
  actual_productivity ~ ., data = train_xy,
  alpha = 1, 
  lambda = lambda_select_lasso
)
summary(fit_lasso_select)
lasso_pred <- predict(fit_lasso_select,x_s_test)
summary(lasso_pred)
#mse
lasso_mse<-mean((y_test - lasso_pred)^2)
#rmse
lasso_rmse<-mean((y_test - lasso_pred)^2)^(1/2)
#mae
lasso_mae<-mae(y_test, lasso_pred)
#Regression Decision Trees
library(rpart)
library(rpart.plot)
set.seed(1)
tree <- rpart(actual_productivity ~ ., data = train_xy)
rpart.plot(tree)
summary(tree)
tree_pred <- predict(tree,x_s_test)
#mse
tree_mse<-mean((y_test - tree_pred)^2)
#rmse
tree_rmse<-mean((y_test - tree_pred)^2)^(1/2)
#mae
tree_mae<-mae(y_test, tree_pred)
#Random Forest Regression
library(randomForest) 
set.seed(1)
rf<- randomForest(
  actual_productivity ~ ., data = train_xy,
  mtry=14, importance=TRUE, ntree = 1000
)
summary(rf)
rf_pred <- predict(rf,x_s_test,type='response')
importance(rf)
varImpPlot(rf)
plot(rf)
#mse
rf_mse<-mean((y_test - rf_pred)^2)
#rmse
rf_rmse<-mean((y_test - rf_pred)^2)^(1/2)
#mae
rf_mae<-mae(y_test, rf_pred)

result <- data.frame (Algorithms  = c('Linear Regression',
                                      'Principal Component Regression',
                                      'Ridge Regression',
                                      'Lasso Regression',
                                      'Regression Decision Trees',
                                      'Random Forest Regression'),
                      MAE = c(lr_mae,pcr_mae_tot[pcr_i],ridge_mae,lasso_mae,tree_mae,
                              rf_mae),
                      MSE = c(lr_mse,pcr_mse_tot[pcr_i],ridge_mse,ridge_mse,lasso_mse,
                              rf_mse),
                      RMSE= c(lr_rmse,pcr_rmse_tot[pcr_i],ridge_rmse,lasso_rmse,tree_rmse,
                              rf_rmse)
)
#result
kable(result)
result_reshaped <- data.frame(Algorithms = result$Algorithms,                           
                          Errors = c(result$MAE, result$MSE, result$RMSE),
                          group = c(rep("MAE", nrow(result)),
                                    rep("MSE", nrow(result)),
                                    rep("RMSE", nrow(result))))

ggplot(result_reshaped, aes(Algorithms, Errors, col = group))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))

