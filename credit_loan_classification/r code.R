#Author : Ye-in, Krystin
setwd("C:/Users/irene/Desktop/team")
#Load dataset
dataset <- read.csv("GermanData.csv")
#Make a copy of dataset
df = dataset
# Cheking missing values
sort(colSums(is.na(df)),decreasing = T)
#Add header
colnames(df) <- c("Status_Checking", "Duration", "Credit_history", "Purpose", "Credit_amount", "Saving", 
                       "Empolyment_duration", "Installment_rate", "Personal_status", "Otherdebtors", 
                       "Residence_Year", "Property", "Age", "Other_installment_plan", "Housing", 
                       "Exisiting_credits", "Job", "Liable_People", "Telephone", "Foreign_worker", 
                       "Credit_default")

#summary and structure
str(df)
summary(df)

#subset numerical dataset
Num_dataset<-df[,c("Duration", "Credit_amount","Installment_rate","Residence_Year", "Age", "Exisiting_credits",
"Liable_People")]

# Handling attibute 9
# Attribute 9:  (qualitative)
# 	      Personal status and sex
# 	      A91 : male   : divorced/separated
# 	      A92 : female : divorced/separated/married
#         A93 : male   : single
# 	      A94 : male   : married/widowed
# 	      A95 : female : single
# Creating a specific feature for sex
df[df$Personal_status == 'A91' |  df$Personal_status == 'A94' | df$Personal_status == 'A93' ,'Sex'] = 'M' 
df[df$Personal_status == 'A92' |  df$Personal_status == 'A95' , 'Sex'] = 'F'
# Adding meaningful descriptions for Personal_status  
df[df$Personal_status == 'A91' | df$Personal_status == 'A92', 'Marital_status'] = 'divorced'
df[df$Personal_status == 'A93' , 'Marital_status'] = 'single'
df[df$Personal_status == 'A94' , 'Marital_status'] = 'married'
df$Personal_status = NULL
df$Sex = as.factor(df$Sex)
df$Marital_status = as.factor(df$Marital_status)

#Change the value of credit_default
df[df$Credit_default==1,'Credit_default'] = 0  # no default
df[df$Credit_default==2,'Credit_default'] = 1  # default

#Change int to factor
df$Credit_default <- as.factor(df$Credit_default)

#summary and structure
str(df)
summary(df)

#Box plots of numerical variables
library(ggplot2)
ggplot(df, aes(x=Credit_default, y=Duration)) + geom_boxplot()
ggplot(df, aes(x=Credit_default, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Credit_default, y=Installment_rate)) + geom_boxplot()
ggplot(df, aes(x=Credit_default, y=Residence_Year)) + geom_boxplot()
ggplot(df, aes(x=Credit_default, y=Age)) + geom_boxplot()
ggplot(df, aes(x=Credit_default, y=Exisiting_credits)) + geom_boxplot()
ggplot(df, aes(x=Credit_default, y=Liable_People)) + geom_boxplot()

#plot of categorical variables
ggplot(df, aes(Status_Checking, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")
ggplot(df, aes(Credit_history, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")
ggplot(df, aes(Purpose, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")
ggplot(df, aes(Otherdebtors, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")
ggplot(df, aes(Property, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")
ggplot(df, aes(Other_installment_plan, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")
ggplot(df, aes(Housing, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")
ggplot(df, aes(Telephone, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")
ggplot(df, aes(Foreign_worker, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")
ggplot(df, aes(Saving, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")
ggplot(df, aes(Empolyment_duration, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")
ggplot(df, aes(Job, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")
ggplot(df, aes(Marital_status, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")
ggplot(df, aes(Sex, ..count..)) + geom_bar(aes(fill = Credit_default), position = "dodge")

#correlation
library(corrplot)
correlation <- cor(Num_dataset, method="spearman")
corrplot(correlation, method = "number", type = "lower")

#test and train set
library(caret)
set.seed(7)
# Creating a balanced split for train/test sets : 30% test 70% train
# This will be usefull to compare the logistic model with others
data_part <- createDataPartition(y = df$Credit_default, p = 0.7, list = F)
X_train = df[data_part,]
X_test = df[-data_part,!(colnames(df) %in% 'Credit_default')]
y_test = df[-data_part,c('Credit_default')]  

#checking the train set
str(X_train)
summary(X_train)

# Checking balance 
table(y_test)
table(X_train$Credit_default)

# Fiting a logistic model
glm.fit = glm(Credit_default~.,data = X_train, family = binomial)
summary(glm.fit)
plot(glm.fit) #plot results
confint(glm.fit) # 95% CI for the coefficients
exp(coef(glm.fit)) # exponentiated coefficients
exp(confint(glm.fit)) # 95% CI for exponentiated coefficients
predict(glm.fit, type="response") # predicted values
residuals(glm.fit, type="deviance") # residuals

# Predicting
glm.pred = predict.glm(glm.fit, newdata = X_test, type = "response")
plot(glm.pred) #plot results
# Calculating accuracy
glm.pred.class = ifelse(glm.pred > 0.5, 1, 0)
mean(y_test == glm.pred.class)
library(pROC)
plot(roc(y_test, glm.pred, direction="<"),col="red", lwd=3, main="")
confusionMatrix(data=factor(glm.pred.class, levels = c(0,1)), reference=factor(y_test, levels = c(0,1)))

#measurePrecisionRecall = function(predict, actual_labels){
  #precision <- sum(predict & actual_labels) / sum(predict)
  #recall <- sum(predict & actual_labels) / sum(actual_labels)
  #fmeasure <- 2 * precision * recall / (precision + recall)
  #cat('\nPrecision:  ')
  #cat(round(precision,2))
  #cat('\n')
  #cat('Recall (sensitivity):     ')
  #cat(round(recall,2))
  #cat('\n')
  #cat('F-score:  ')
  #cat(round(fmeasure,2))
  #cat('\n')
#}

#measurePrecisionRecall(glm.pred.class, y_test)

#Bayesian Regression


#MCMC & Jags


