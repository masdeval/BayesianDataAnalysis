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

#Scatterplot of numerical variables
library(ggplot2)
ggplot(df, aes(x=Credit_amount, y=Duration)) + geom_point()
ggplot(df, aes(x=Credit_amount, y=Age)) + geom_point()


#Change int to factor
ggplot(df, aes(x=Status_Checking, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Credit_history, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Purpose, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Otherdebtors, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Property, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Other_installment_plan, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Housing, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Telephone, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Foreign_worker, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Saving, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Empolyment_duration, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Job, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Marital_status, y=Credit_amount)) + geom_boxplot()
ggplot(df, aes(x=Sex, y=Credit_amount)) + geom_boxplot()

#correlation
library(corrplot)
correlation <- cor(Num_dataset, method="spearman")
corrplot(correlation, method = "number", type = "lower")


#test and train set
library(caret)
set.seed(7)
# Creating a balanced split for train/test sets : 30% test 70% train
# This will be usefull to compare the logistic model with others
data_part <- createDataPartition(y = df$Credit_amount, p = 0.7, list = F)
X_train = df[data_part,]
X_test = df[-data_part,]

#checking the train set
str(X_train)
summary(X_train)
#check balance
nrow(X_test)
nrow(X_train)

# Fiting a logistic model
lm.fit = lm(Credit_amount~.,data = X_train)
summary(lm.fit)
plot(lm.fit)
trainanova <- anova(lm.fit)
summary(trainanova)

#chose significant variables with p-value lower than 0.001
significantVars <- c("Status_Checking", "Duration", "Purpose", "Installment_rate", "Property", "Job", "Credit_amount")

#histogram of credit amount
hist(df$Credit_amount, main="Credit Amount", xlab="Credit Amount", border="blue")

#multiregression
  #subset of dataset on the basis of the significant numerical variables
    train_complete_sub <- X_train[,significantVars]
    test_complete_sub <- X_test[,significantVars]
  #Multiple Linear Regression
    Credit_Norm_Full = lm(log(Credit_amount) ~ . , data = train_complete_sub)
    print(summary(Credit_Norm_Full))
    plot(Credit_Norm_Full)
  #MSE and RMSE Calculation  
    testing_y<- test_complete_sub$Credit_amount
    predicted_y<- predict(Credit_Norm_Full,test_complete_sub)
    MSE<- mean((testing_y-exp(predicted_y))^2,na.rm = T)
    RMSE <- MSE^0.5
    RMSE

# Bayesian Approach
  #install.packages('BAS')
  library(BAS)
  bma_Credit_amount = bas.lm(log(Credit_amount) ~ ., data = X_train[ ,significantVars], 
                             prior = "BIC", modelprior = uniform(), method = "MCMC")
  
  print(summary(bma_Credit_amount))
  
# Posterior Mean, Standard Deviation and Posterior Probabilities 
  estimatorResults <- data.frame(BMA=double(),BPM=double(),MPM=double(),HPM=double(),stringsAsFactors=FALSE)
  
  for (estimatorName in colnames(estimatorResults)) {
    print(coef(bma_Credit_amount,estimator = estimatorName))
  }
  
  
  ## 95% credible intervals for these coefficients
  confint(coef(bma_Credit_amount,estimator = estimatorName),level = 0.95)
  
  yPred <- fitted(bma_Credit_amount, type = "response", estimator = "BMA")
  
  exp_yPred <- exp(yPred)
  
  plot(train_complete_sub$Credit_amount,exp_yPred,col=c('blue', 'green'), xlab = "Actual Credit Amount",
       ylab = "Predicted Credit Amount", main = "Predicted Vs Actual Credit Amount", xaxt="n")
  
  # Plotting the training dataset actual and predicted values
  trainDF <- cbind(train_complete_sub,yPred=as.data.frame(exp_yPred))
  p <- ggplot(aes(x=Credit_amount,y=exp_yPred), data=trainDF) + xlab("Credit Amount") + 
    ylab("Predicted Credit Amount") +  ggtitle("Predicted Vs Actual Credit Amount")
  p1 <- p + geom_point() + geom_smooth()
  plot(p1)
  for (estimatorName in colnames(estimatorResults)) {
    testing_y<- test_complete_sub$Credit_amount
    y_pred = predict(bma_Credit_amount, test_complete_sub, estimator=estimatorName)$fit
    MSE <- mean((testing_y-exp(y_pred))^2,na.rm = T)
    RMSE <- MSE^0.5
    print(paste0("Root Mean Square Error ",estimatorName," ",RMSE,sep = ""))
  }
  
  
