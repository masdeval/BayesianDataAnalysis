
#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
#rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 


# Load The data 
#myData = data.frame(read.csv("~/GWU/Bayesian Methods Data Analysis/exercise2/glucoseData.csv"))
df = data.frame(read.csv('GermanData.csv'))
colnames(df) <- c("Status_Checking", "Duration", "Credit_history", "Purpose", "Credit_amount",
                  "Saving","Empolyment_duration", "Installment_rate", "Personal_status",
                  "Otherdebtors", "Residence_Year", "Property", "Age", "Other_installment_plan",
                  "Housing", "Exisiting_credits", "Job", "Liable_People", "Telephone",
                  "Foreign_worker", "Credit_default")
df[df$Credit_default==1, 'Credit_default'] = 0
df[df$Credit_default==2, 'Credit_default'] = 1

# N.B.: The functions below expect the data to be a data frame, 
# with one component named y being a vector of integer 0,1 values,
# and one component named s being a factor of subject identifiers.
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("credit_loan_solution.R")
#------------------------------------------------------------------------------- 
# Optional: Specify filename root and graphical format for saving output.
# Otherwise specify as NULL or leave saveName and saveType arguments 
# out of function calls.
fileNameRoot = "credit_loan_" 
graphFileType = "pdf" 
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
mcmcCoda = genMCMC(data=df, sName="Saving", yName="Credit_default", numSavedSteps=50000 , saveName=fileNameRoot )
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
                saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC_Kappa_Omega( mcmcCoda , compVal=0.5 , #rope=c(0.45,0.55) ,
                        compValDiff=0.0 ,  diffIdVec=seq(1,5), #ropeDiff = c(-0.05,0.05) ,
                        saveName=fileNameRoot )
# Display posterior information:

plotMCMC_Kappa_Omega( mcmcCoda , data=df , compVal=0.5 , sName="Saving" , yName="Credit_default", #rope=c(0.45,0.55) ,
          compValDiff=0.0 ,  diffIdVec=seq(1,5), #ropeDiff = c(-0.05,0.05) ,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 

graphics.off() # This closes all of R's graphics windows.