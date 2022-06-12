library(dplyr)

library(ggplot2)

#load data
house_train <- data.table::fread(here::here('linear regression','house_train.csv')) %>%
  select(-V1)
house_val <- data.table::fread(here::here('linear regression','house_val.csv')) %>%
  select(-V1)
house_test <- data.table::fread(here::here('linear regression','house_test.csv')) %>%
  select(-V1)


#standardized data (only x variables and log(y))
set.seed(17)
house_train_scaled <- as.data.frame(scale(house_train[,1:17])) %>%
  cbind(price=house_train$price) %>%
  mutate(log_price = log(price)) %>%
  select(-price)

house_val_scaled <- as.data.frame(scale(house_val[,1:17])) %>%
  cbind(price=house_val$price) %>%
  mutate(log_price = log(price)) %>%
  select(-price)

house_test_scaled <- as.data.frame(scale(house_test[,1:17])) %>%
  cbind(price=house_test$price) %>%
  mutate(log_price = log(price)) %>%
  select(-price)

#data quality
skimr::skim(house_train_scaled)

#distribution of scaled dependent var
var <- "log_price"
ggplot(data=house_train_scaled, aes(x = !!sym(var))) + 
  geom_histogram() + 
  theme_minimal() + 
  labs(title = glue::glue("Histogram for variable {var}"))

#select only useful variables based on LR
house_train_scaled_filter <- house_train_scaled %>%
  select(-c(bathrooms, sqft_lot, sqft_lot15, sqft_above, zipcode,lat,long))
house_val_scaled_filter <- house_val_scaled %>%
  select(-c(bathrooms, sqft_lot, sqft_lot15, sqft_above, zipcode,lat,long))
house_test_scaled_filter <- house_test_scaled %>%
  select(-c(bathrooms, sqft_lot, sqft_lot15, sqft_above, zipcode,lat,long))

rm(house_test,house_test_scaled,house_train,house_train_scaled,house_val,house_val_scaled)

###############################################################################################
#2 classes
#bin the dependent variables [base::cut (convert numerical to categorical)]
qplot(y=log_price, data = house_train_scaled_filter, colour='red')
bins <- cut(house_train_scaled_filter$log_price, 2, include.lowest = TRUE, labels = c('Low','High'))
bins
#log_price bins (train)
#1. Low: [11.3,13.5]
#2. High: (13.5,15.8]
val_bins <- cut(house_val_scaled_filter$log_price, 2, include.lowest = TRUE, labels = c('Low','High'))
#log_price bins (val)
#1. Low: [11.3,13.5]
#2. High: (13.5,15.9]

#check class distribution
table(bins)
table(val_bins)

#combine x and y variables
house_train_scaled_filter_class <- cbind(house_train_scaled_filter,as.data.frame(bins)) %>%
  select(-log_price)
house_val_scaled_filter_class <- cbind(house_val_scaled_filter,as.data.frame(val_bins)) %>%
  select(-log_price)

#boxplot to check the distributions in each variable for the 3 classes
#[2] topic type
par(mfrow=c(3,4))
for (i in 1:ncol(house_train_scaled_filter_class)){
  boxplot(house_train_scaled_filter_class[,i]~bins,data=house_train_scaled_filter_class, main=colnames(house_train_scaled_filter_class[i]), 
          xlab="Categories", ylab=colnames(house_train_scaled_filter_class[i]))
}

#fit into logistic regression model
glm.fit=glm(bins~.,data=house_train_scaled_filter_class,family=binomial)
summary(glm.fit)
# @ using glmnet [glmnet(x,y,family)]
# glmnet.fit = glmnet::glmnet(x= as.matrix(house_train_scaled_filter), y= bins, family='binomial')
# par(mfrow=c(1,1))
# plot(glmnet.fit)

#predict validation data
glm.probs=predict(glm.fit,newdata = house_val_scaled_filter_class,type="response") 
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"High","Low")
glm.pred=as.factor(glm.pred)
#confusion matrix
attach(house_val_scaled_filter_class)
caret::confusionMatrix(glm.pred, house_val_scaled_filter_class$val_bins)
#table(glm.pred,val_bins)
#mean classification performance
mean(glm.pred==val_bins)


#fit into LDA
lda.fit=MASS::lda(bins~.,data=house_train_scaled_filter_class)
lda.fit
plot(lda.fit)
#predict validation data
lda.pred=predict(lda.fit,house_val_scaled_filter_class)
#glimpse at the predicted value
data.frame(lda.pred)[1:5,]
caret::confusionMatrix(lda.pred$class, house_val_scaled_filter_class$val_bins)
#table(lda.pred$class,house_val_scaled_filter_class$val_bins)
mean(lda.pred$class==house_val_scaled_filter_class$val_bins)


#fit into KNN
library(class)
#knn(train, test, train_classification, k=# of neighbours considered)
knn.pred=knn(house_train_scaled_filter_class[,1:10],house_val_scaled_filter_class[,1:10],house_train_scaled_filter_class$bins,k=1)
caret::confusionMatrix(knn.pred, house_val_scaled_filter_class$val_bins)
#table(knn.pred,house_val_scaled_filter_class$val_bins)
mean(knn.pred==house_val_scaled_filter_class$val_bins)

#package: e1071
#naive bayes classification (quick learner for huge datasets) [works for 2 levels]
library(e1071)
nb.fit <- caret::train(house_train_scaled_filter_class[,1:10],house_train_scaled_filter_class[,11],'nb')
nb.pred <- predict(nb.fit,newdata = house_val_scaled_filter_class)
data.frame(nb.pred)[1:5,]
caret::confusionMatrix(nb.pred, house_val_scaled_filter_class$val_bins)
#table(nb.pred,house_val_scaled_filter_class$val_bins)
mean(nb.pred==house_val_scaled_filter_class$val_bins)

#############################################################################################
#multi-classes
qplot(y=log_price, data = house_train_scaled_filter, colour='red')
bins <- cut(house_train_scaled_filter$log_price, 3, include.lowest = TRUE, labels = c('Low','Medium','High'))
bins
#log_price bins (train)
#1. Low: [11.3,12.8]
#2. Medium: (12.8,14.3]
#3. High: (14.3,15.8]
val_bins <- cut(house_val_scaled_filter$log_price, 3, include.lowest = TRUE, labels = c('Low','Medium','High'))
#log_price bins (val)
#1. Low: [11.3,13.5]
#2. Medium: (12.8,14.3]
#3. High: (13.5,15.9]

#check class distribution
table(bins)
table(val_bins)

#combine x and y variables
house_train_scaled_filter_3class <- cbind(house_train_scaled_filter,as.data.frame(bins)) %>%
  select(-log_price)
house_val_scaled_filter_3class <- cbind(house_val_scaled_filter,as.data.frame(val_bins)) %>%
  select(-log_price)

#boxplot to check the distributions in each variable for the 3 classes
#[2] topic type
par(mfrow=c(3,4))
for (i in 1:ncol(house_train_scaled_filter_3class)){
  boxplot(house_train_scaled_filter_3class[,i]~bins,data=house_train_scaled_filter_3class, main=colnames(house_train_scaled_filter_3class[i]), 
          xlab="Categories", ylab=colnames(house_train_scaled_filter_3class[i]))
}

#fit into logistic regression model
glmnet.3fit_a = glmnet::glmnet(x=as.matrix(house_train_scaled_filter),y=bins,family="multinomial")
summary(glmnet.3fit_a)
par(mfrow=c(1,1))
plot(glmnet.3fit_a)
#predict validation data
glmnet.3prob_a = predict(glmnet.3fit_a,as.matrix(house_val_scaled_filter), type="response")
glmnet.3prob_a[1:5]
glmnet.3pred_a <- colnames(glmnet.3prob_a)[apply(glmnet.3prob_a,1,which.max)] #how to convert probs to class
plot(glmnet.3pred_a, val_bins)
#confusion matrix
attach(house_val_scaled_filter_class)
caret::confusionMatrix(glm.pred, house_val_scaled_filter_class$val_bins)
#table(glm.pred,val_bins)
#mean classification performance
mean(glm.3pred_a ==val_bins)


#fit into LDA
lda.3fit=MASS::lda(bins~.,data=house_train_scaled_filter_3class)
lda.3fit
plot(lda.3fit)
#predict validation data
lda.3pred=predict(lda.3fit,house_val_scaled_filter_3class)
#glimpse at the predicted value
data.frame(lda.3pred)[1:5,]
caret::confusionMatrix(lda.3pred$class, house_val_scaled_filter_3class$val_bins)
#table(lda.pred$class,house_val_scaled_filter_class$val_bins)
mean(lda.3pred$class==house_val_scaled_filter_3class$val_bins)

#fit into KNN
library(class)
#knn(train, test, train_classification, k=# of neighbours considered)
knn.3pred=knn(house_train_scaled_filter_3class[,1:10],house_val_scaled_filter_3class[,1:10],house_train_scaled_filter_3class$bins,k=1)
caret::confusionMatrix(knn.3pred, house_val_scaled_filter_3class$val_bins)
#table(knn.pred,house_val_scaled_filter_class$val_bins)
mean(knn.3pred==house_val_scaled_filter_3class$val_bins)

#package: e1071
#naive bayes classification (quick learner for huge datasets) [works for 2 levels]
library(e1071)
nb.3fit <- caret::train(house_train_scaled_filter_3class[,1:10],house_train_scaled_filter_3class[,11],'nb')
nb.3pred <- predict(nb.3fit,newdata = house_val_scaled_filter_3class)
data.frame(nb.pred)[1:5,]
caret::confusionMatrix(nb.3pred, house_val_scaled_filter_3class$val_bins)
#table(nb.pred,house_val_scaled_filter_class$val_bins)
mean(nb.pred==house_val_scaled_filter_class$val_bins)
