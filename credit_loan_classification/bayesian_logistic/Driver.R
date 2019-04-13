#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
#rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 
#.............................................................................
# myData = read.csv( file="CondLogistRegData1.csv" )
# yName = "Y" ; xName = c("X1","X2")
# fileNameRoot = "CondLogistRegData1-Model2-" 
# numSavedSteps=5000 ; thinSteps=5
#.............................................................................

# Loading the dataset with the following fields:
# c("Status_Checking", "Duration", "Credit_history", "Purpose", "Credit_amount",
# "Saving","Empolyment_duration", "Installment_rate", 
# "Otherdebtors", "Residence_Year", "Property", "Age", "Other_installment_plan",
# "Housing", "Exisiting_credits", "Job", "Liable_People", "Telephone",
# "Foreign_worker", "Sex", "Marital_status")

myData = read.csv( file="german_default.csv" )
yName = "Credit_default" 
xName_numeric = c("Duration","Credit_amount","Installment_rate","Age","Exisiting_credits")

library(dummies)
dummies = dummy.data.frame(myData, all = FALSE)
Status_checking = c(colnames(dummies[1:4])) 
Credit_history = c(colnames(dummies[5:9]))
Purpose = c(colnames(dummies[10:19]))
Savings = c(colnames(dummies[20:24]))
Employment = c(colnames(dummies[25:29]))
Marital_status = c(colnames(dummies[53:55]))
Job = c(colnames(dummies[43:46]))

xName_categorical = c(Status_checking,Credit_history,Purpose,Savings,Employment,Marital_status,Job)

# Concatening the data
newData = cbind(myData[,xName_numeric],dummies[,xName_categorical])

# Removing categorical features from the dataset
# newData$Status_Checking = newData$Credit_history = newData$Purpose = newData$Saving = newData$Empolyment_duration =
#   newData$Otherdebtors = newData$Property = newData$Other_installment_plan = newData$Housing = newData$Job =
#   newData$Telephone = newData$Foreign_worker = newData$Sex = newData$Marital_status = NULL


fileNameRoot = "loan-default-" 
numSavedSteps=5000 ; thinSteps=5
#.............................................................................
# myData = read.csv( file="SoftmaxRegData1.csv" )
# yName = "Y" ; xName = c("X1","X2")
# fileNameRoot = "SoftmaxRegData1-Model2-" 
# numSavedSteps=5000 ; thinSteps=5
#.............................................................................
# myData = read.csv( file="SoftmaxRegData2.csv" )
# yName = "Y" ; xName = c("X1","X2")
# fileNameRoot = "SoftmaxRegData2-Model2-" 
# numSavedSteps=5000 ; thinSteps=5
#.............................................................................
graphFileType = "eps" 
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Solution.R")
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
#startTime = proc.time()
mcmcCoda = genMCMC( data=newData, xName=c(xName_categorical, xName_numeric), yName=yName, numSavedSteps=numSavedSteps, thinSteps=thinSteps, saveName=fileNameRoot )
#stopTime = proc.time()
#duration = stopTime - startTime
#show(duration)
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
#parameterNames = varnames(mcmcCoda) # get all parameter names
#for ( parName in grep( "^beta" , parameterNames , value=TRUE ) ) {
#  diagMCMC( codaObject=mcmcCoda , parName=parName , 
#            saveName=fileNameRoot , saveType=graphFileType )
#}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC(mcmcCoda, data=newData, xName=c(xName_categorical,xName_numeric), yName=yName, 
         pairsPlot=TRUE, showCurve=FALSE, saveName=fileNameRoot, saveType=graphFileType)
#------------------------------------------------------------------------------- 
graphics.off() # This closes all of R's graphics windows.
