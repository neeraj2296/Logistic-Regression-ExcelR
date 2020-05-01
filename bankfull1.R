#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Importing the data set <<<<<<<<<<<<<<<<<<<<<
bank <- read.csv(file.choose())
banks <- bank
View(bank)
dim(bank)
attach(bank)
summary(bank)
#sum(is.na(bank))

#Converting the categorical data into numerical
#converting job variable
banks$job<-as.integer(as.factor(banks$job))
banks$marital<-as.integer(as.factor(banks$marital))
banks$education<-as.integer(as.factor(banks$education))
banks$default<-as.integer(as.factor(banks$default))
banks$housing<-as.integer(as.factor(banks$housing))
banks$loan<-as.integer(as.factor(banks$loan))
banks$contact<-as.integer(as.factor(banks$contact))
banks$month<-as.integer(as.factor(banks$month))
banks$poutcome<-as.integer(as.factor(banks$poutcome))
banks$y<-as.integer(as.factor(banks$y))

banks$y[banks$y %in% "1"] <- "0"
banks$y[banks$y %in% "2"] <- "1"

banks$y <- as.numeric(banks$y)
# Preparing a linear regression
mod_lm <- lm(y~.,data=banks)
summary(mod_lm)
pred1 <- predict(mod_lm,banks)
plot(banks$y,pred1)
# We can no way use the linear regression technique to classify the data
plot(pred1)

# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
mod_glm <- glm(y~.,data = banks,family = binomial("logit"), maxit = 100)
summary(mod_glm)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(mod_glm))

# Confusion matrix table 
prob1 <- predict(mod_glm,banks,type = "response")
summary(prob1)
prob1

# We are going to use NULL and Residual Deviance to compare the between different models
# Confusion matrix and considering the threshold value as 0.5 
confusion1<-table(prob1>0.5,banks$y)
confusion1

# Model Accuracy 
Accuracy<-sum(diag(confusion1)/sum(confusion1))
Accuracy 

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob1>=0.5,1,0)
yes_no <- ifelse(prob1>=0.5,"yes","no")


banks[,"prob1"] <- prob1
banks[,"pred_values"] <- pred_values
banks[,"yes_no"] <- yes_no

View(banks[,c(17,18:19)])

table(banks$y,banks$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
rocrpred<-prediction(prob1,banks$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
