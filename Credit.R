#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Importing the data set <<<<<<<<<<<<<<<<<<<<<
credit <- read.csv(file.choose())
attach(credit)
View((credit))

credit <- credit[,-1]#removal of index column in the dataset

#Converting the categorical data into numerical
#converting card variable
credit$card <- as.integer(as.factor(credit$card))
credit$card[credit$card %in% "1"] <- "0"
credit$card[credit$card %in% "2"] <- "1"
credit$card <- as.numeric(credit$card)
#converting owner variable
credit$owner <- as.integer(as.factor(credit$owner))
credit$owner[credit$owner %in% "1"] <- "0"
credit$owner[credit$owner %in% "2"] <- "1"
credit$owner <- as.numeric(credit$owner)
#selfemp
credit$selfemp <- as.integer(as.factor(credit$selfemp))
credit$selfemp[credit$selfemp %in% "1"] <- "0"
credit$selfemp[credit$selfemp %in% "2"] <- "1"
credit$selfemp <- as.numeric(credit$selfemp)
# Preparing a linear regression
mod_lm <- lm(card~.,data=credit)
summary(mod_lm)
pred1 <- predict(mod_lm,credit)
plot(credit$card,pred1)
# We can no way use the linear regression technique to classify the data
plot(pred1)


# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
mod_glm <- glm(card~.,data=credit)
summary(mod_glm)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(mod_glm))

# Confusion matrix table 
prob1 <- predict(mod_glm,credit,type = "response")
summary(prob1)
prob1

# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion1<-table(prob1>0.5,credit$card)
confusion1

# Model Accuracy 
Accuracy<-sum(diag(confusion1)/sum(confusion1))
Accuracy 

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob1>=0.5,1,0)
yes_no <- ifelse(prob1>=0.5,"yes","no")

# Creating new column to store the above values
credit[,"prob1"] <- prob1
credit[,"pred_values"] <- pred_values
credit[,"yes_no"] <- yes_no

View(credit[,c(17,18:19)])

table(credit$y,credit$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity



# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
rocrpred<-prediction(prob1,credit$card)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-1.01,1.7))


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

