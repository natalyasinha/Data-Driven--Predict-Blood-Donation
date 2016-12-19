
My.Location <- ""
setwd(My.Location)
train.blood <- read.csv('Training.csv')
test.blood <- read.csv("Test.csv")

str(train.blood)

table(train.blood$Months.since.Last.Donation,train.blood$Made.Donation.in.March.2007)
table(train.blood$Months.since.First.Donation,train.blood$Made.Donation.in.March.2007)
table(train.blood$Made.Donation.in.March.2007)

train.blood$Total.Volume.Donated..c.c.. <- log(train.blood$Total.Volume.Donated..c.c..)
test.blood$Total.Volume.Donated..c.c.. <- log(test.blood$Total.Volume.Donated..c.c..)

set.seed(123)
d <- sort(sample(nrow(train.blood), nrow(train.blood)*0.8))
dev.blood<-train.blood[d,]
val.blood<-train.blood[-d,]


################### Logistic Regression Models ##############################
lm_training_blood <- glm(Made.Donation.in.March.2007~ ., data=dev.blood[,-1], family="binomial")
summary(lm_training_blood) # estimates

exp(lm_training_blood$coefficients)
####confidence intervals for the coefficient estimates
confint(lm_training_blood)
#######performance checking on development/training data#####
library(ROCR) # For ROC, AUC calculation
library(rms)
?predict
?prediction
?performance
prob <- predict(lm_training_blood, newdata=dev.blood[,-1], type="response") # predicted probabilities
pred <- prediction(prob,dev.blood$Made.Donation.in.March.2007)
perf <- performance(pred,measure = "tpr",x.measure = "fpr")
plot(perf)

############performance checking on validation/test data####
prob <- predict(lm_training_blood, newdata=val.blood[,-1], type="response") # predicted probabilities
pred <- prediction(prob,val.blood$Made.Donation.in.March.2007)
perf <- performance(pred,measure = "tpr",x.measure = "fpr")
plot(perf)

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc ####0.6426711
#######confusion matrix##
conf_data<-as.data.frame(cbind(prob, val.blood$Made.Donation.in.March.2007))
str(conf_data)
table(conf_data$prob>0.5,val.blood$Made.Donation.in.March.2007)
m<-table(conf_data$prob>0.5,val.blood$Made.Donation.in.March.2007)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])


############performance checking on test data####
prob <- predict(lm_training_blood, newdata=test.blood[,-1], type="response") # predicted probabilities
output_test1 <- cbind(test.blood[,1],prob)
colnames(output_test1) <- c("","Made Donation in March 2007")
write.csv(output_test1, "SubmissionFormat_abheek2.csv", row.names=FALSE)



################### Random Forest ##############################
library(randomForest)
?randomForest
rf = randomForest(Made.Donation.in.March.2007~ ., data=train.blood[,-1], mtry=2, ntree=100,importance=TRUE)
varImpPlot(rf)
############performance checking on test data####
prob <- predict(rf, newdata=test.blood[,-1], type="response") # predicted probabilities
output_test1 <- cbind(test.blood[,1],prob)
colnames(output_test1) <- c("","Made Donation in March 2007")
write.csv(output_test1, "SubmissionFormat_abheek_rf.csv", row.names=FALSE)


################### forest of conditional inference trees. ##############################
# install.packages('party')
library(party)

set.seed(415)
fit <- cforest(Made.Donation.in.March.2007~ .,
                 data = train.blood[,-1], 
                 controls=cforest_unbiased(ntree=2000, mtry=3))
prob <- predict(fit, test.blood[,-1], OOB=TRUE, type = "response")
output_test1 <- cbind(test.blood[,1],prob)
colnames(output_test1) <- c("","Made Donation in March 2007")
write.csv(output_test1, "SubmissionFormat_abheek_party.csv", row.names=FALSE)



################### SVM ##############################
library(e1071)
set.seed(123)
tune.out=tune(svm, Made.Donation.in.March.2007~., data=dev.blood[,-1], kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)

############performance checking on validation/test data####
prob <- predict(tune.out$best.model, newdata=val.blood[,-1], type="response") # predicted probabilities
pred <- prediction(prob,val.blood$Made.Donation.in.March.2007)
perf <- performance(pred,measure = "tpr",x.measure = "fpr")
plot(perf)

###### Predicting Test fit from the best model

conf_data<-as.data.frame(cbind(prob, val.blood$Made.Donation.in.March.2007))
str(conf_data)
table(conf_data$prob>0.5,val.blood$Made.Donation.in.March.2007)
m<-table(conf_data$prob>0.5,val.blood$Made.Donation.in.March.2007)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])


################### ADA Boost Models ##############################

set.seed(123)
d <- sort(sample(nrow(train.blood), nrow(train.blood)*0.8))
dev.blood<-train.blood[d,]
val.blood<-train.blood[-d,]

dev.blood$Made.Donation.in.March.2007 <- as.factor(dev.blood$Made.Donation.in.March.2007)
val.blood$Made.Donation.in.March.2007 <- as.factor(val.blood$Made.Donation.in.March.2007)

library(rpart)
library(adabag)
mfinal <- 10
maxdepth <- 5
str(train.blood)
?boosting
?rpart.control
train.adaboost.blood <- boosting(Made.Donation.in.March.2007 ~.,data=dev.blood[,-1],mfinal=mfinal,
                           control=rpart.control(maxdepth=maxdepth))
train.adaboost.blood.pred <- predict.boosting(train.adaboost.blood,newdata=val.blood[,-1])
train.adaboost.blood.pred$confusion
train.adaboost.blood.pred$error
actual.prob <- as.data.frame(train.adaboost.blood.pred$prob)
actual_prob <- ifelse(actual.prob$V1 > actual.prob$V2,actual.prob$V1,1-actual.prob$V1) 

train.adaboost.blood.pred <- predict.boosting(train.adaboost.blood,newdata=test.blood[,-1])
actual.prob <- as.data.frame(train.adaboost.blood.pred$prob)
actual_prob <- ifelse(actual.prob$V1 > actual.prob$V2,1-actual.prob$V1,actual.prob$V1) 
output_test1 <- cbind(test.blood[,1],actual_prob)
colnames(output_test1) <- c(" ","Made Donation in March 2007")
write.csv(output_test1, "SubmissionFormat_abheek_ada.csv", row.names=FALSE)

