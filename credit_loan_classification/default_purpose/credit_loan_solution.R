source("DBDA2E-utilities.R")
#===============================================================================


genMCMC = function(data, sName="Purpose", yName="Credit_default", numSavedSteps=50000 , saveName=NULL ) { 
  require(rjags)
  #-----------------------------------------------------------------------------
  # THE DATA
  # Implement me
  y = data[,yName]
  s = as.numeric(data[,sName])
  Nsubj = length(unique(s))
  
  z = aggregate(y , by=list(s) , FUN=sum)$x
  N = aggregate(rep(1, length(y)), by=list(s), FUN=sum)$x
  dataList = list(
    z = z ,
    N = N ,
    Nsubj = Nsubj
  )
  
  modelString2 = "

    model {
      for ( s in 1:Nsubj ) {
        z[s] ~ dbin( theta[s] , N[s] )
        theta[s]  ~ dbeta( omega*(kappa-2)+1 , (1-omega)*(kappa-2)+1 )
      }
    omega ~ dbeta( 1 , 1 ) # broad uniform
    kappa = kappaMinusTwo + 2
    #kappaMinusTwo ~ dgamma( 0.01 , 0.01 )  # mean=1 , sd=10 (generic vague)
    kappaMinusTwo ~ dgamma( 1.105125 , 0.1051249 )  # mode=1 , sd=10 
  
    }
"

  modelString1 = "

    model {
      for ( s in 1:Nsubj ) {
        z[s] ~ dbin( theta[s] , N[s] )
        theta[s]  ~ dbeta(1, 1)
      }
    }
"
 
  # close quote for modelString
  writeLines( modelString2 , con="TEMPmodel.txt" )
  #-----------------------------------------------------------------------------
  
  
  # INTIALIZE THE CHAINS.
  # Initial values of MCMC chains based on data:
  # Option 1: Use single initial value for all chains:
  #  thetaInit = rep(0,Nsubj)
  #  for ( sIdx in 1:Nsubj ) { # for each subject
  #    includeRows = ( s == sIdx ) # identify rows of this subject
  #    yThisSubj = y[includeRows]  # extract data of this subject
  #    thetaInit[sIdx] = sum(yThisSubj)/length(yThisSubj) # proportion
  #  }
  #  initsList = list( theta=thetaInit )
  # Option 2: Use function that generates random values near MLE:
  
  # initsList = function() {
  #   thetaInit = rep(0,Nsubj)
  #   for ( sIdx in 1:Nsubj ) { # for each subject
  #     includeRows = ( s == sIdx ) # identify rows of this subject
  #     yThisSubj = y[includeRows]  # extract data of this subject
  #     resampledY = sample( yThisSubj , replace=TRUE ) # resample
  #     thetaInit[sIdx] = sum(resampledY)/length(resampledY)
  #   }
  #   thetaInit = 0.001+0.998*thetaInit # keep away from 0,1
  #   return( list( theta=thetaInit ) )
  # }

  initsList = function() {
    thetaInit = rep(0,Nsubj)
    for ( sIdx in 1:Nsubj ) { # for each subject
      includeRows = ( s == sIdx ) # identify rows of this subject
      yThisSubj = y[includeRows]  # extract data of this subject
      resampledY = sample( yThisSubj , replace=TRUE ) # resample
      thetaInit[sIdx] = sum(resampledY)/length(resampledY) 
    }
    thetaInit = 0.001+0.998*thetaInit # keep away from 0,1
    meanThetaInit = mean( thetaInit )
    kappaInit = 100 # lazy, start high and let burn-in find better value
    return( list( theta=thetaInit , omega=meanThetaInit , 
                  kappaMinusTwo=kappaInit-2 ) )
  }
  
  
  
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  
  #parameters = c( "theta")     # The parameters to be monitored
  parameters = c( "theta","omega","kappa") # The parameters to be monitored
  
  adaptSteps = 500             # Number of steps to adapt the samplers
  burnInSteps = 500            # Number of steps to burn-in the chains
  nChains = 4                  # nChains should be 2 or more for diagnostics 
  thinSteps = 1
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )
  # Create, initialize, and adapt the model:
  jagsModel = jags.model( "TEMPmodel.txt" , 
                          data=dataList , inits=initsList , 
                          n.chains=nChains , n.adapt=adaptSteps )
  # Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter=burnInSteps )
  # The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                              n.iter=nIter , thin=thinSteps )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
  
  
} # end function

#===============================================================================

smryMCMC_Theta = function(  codaSamples , compVal=0.5 , rope=NULL , 
                      compValDiff=0.0 , ropeDiff=NULL , saveName=NULL) {
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  Ntheta = length(grep("theta",colnames(mcmcMat)))
  summaryInfo = NULL
  rowIdx = 0
  for ( tIdx in 1:Ntheta ) {
    parName = paste0("theta[",tIdx,"]")
    summaryInfo = rbind( summaryInfo , 
      summarizePost( mcmcMat[,parName] , compVal=compVal , ROPE=rope ) )
    rowIdx = rowIdx+1
    rownames(summaryInfo)[rowIdx] = parName
  }
  for ( t1Idx in 1:(Ntheta-1) ) {
    for ( t2Idx in (t1Idx+1):Ntheta ) {
      parName1 = paste0("theta[",t1Idx,"]")
      parName2 = paste0("theta[",t2Idx,"]")
      summaryInfo = rbind( summaryInfo , 
        summarizePost( mcmcMat[,parName1]-mcmcMat[,parName2] ,
                       compVal=compValDiff , ROPE=ropeDiff ) )
      rowIdx = rowIdx+1
      rownames(summaryInfo)[rowIdx] = paste0(parName1,"-",parName2)
    }
  }

  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  show( summaryInfo )
  return( summaryInfo )
}

smryMCMC_Kappa_Omega = function(  codaSamples , compVal=0.5 , rope=NULL , 
                      diffIdVec=NULL , compValDiff=0.0 , ropeDiff=NULL , 
                      saveName=NULL ) {
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  Ntheta = length(grep("theta",colnames(mcmcMat)))
  summaryInfo = NULL
  rowIdx = 0
  # overall omega:
  summaryInfo = rbind( summaryInfo , 
                       summarizePost( mcmcMat[,"omega"] ,
                                      compVal=compVal , ROPE=rope ) )
  rowIdx = rowIdx+1
  rownames(summaryInfo)[rowIdx] = "omega"
  # kappa:
  summaryInfo = rbind( summaryInfo , 
                       summarizePost( mcmcMat[,"kappa"] ,
                                      compVal=NULL , ROPE=NULL ) )
  rowIdx = rowIdx+1
  rownames(summaryInfo)[rowIdx] = "kappa"
  # individual theta's:
  for ( tIdx in 1:Ntheta ) {
    parName = paste0("theta[",tIdx,"]")
    summaryInfo = rbind( summaryInfo , 
                         summarizePost( mcmcMat[,parName] , compVal=compVal , ROPE=rope ) )
    rowIdx = rowIdx+1
    rownames(summaryInfo)[rowIdx] = parName
  }
  # differences of individual theta's:
  if ( !is.null(diffIdVec) ) {
    Nidx = length(diffIdVec)
    for ( t1Idx in 1:(Nidx-1) ) {
      for ( t2Idx in (t1Idx+1):Nidx ) {
        parName1 = paste0("theta[",diffIdVec[t1Idx],"]")
        parName2 = paste0("theta[",diffIdVec[t2Idx],"]")
        summaryInfo = rbind( summaryInfo , 
                             summarizePost( mcmcMat[,parName1]-mcmcMat[,parName2] ,
                                            compVal=compValDiff , ROPE=ropeDiff ) )
        rowIdx = rowIdx+1
        rownames(summaryInfo)[rowIdx] = paste0(parName1,"-",parName2)
      }
    }
  }
  # save:
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  show( summaryInfo )
  return( summaryInfo )
}


#===============================================================================

plotMCMC_Theta = function( codaSamples , data , sName="s" , yName="y", compVal=0.5 , rope=NULL , 
                     compValDiff=0.0 , ropeDiff=NULL , 
                     saveName=NULL , saveType="jpg" ) {
  #-----------------------------------------------------------------------------
  # N.B.: This function expects the data to be a data frame, 
  # with one component named y being a vector of integer 0,1 values,
  # and one component named s being a factor of subject identifiers.
  y = data[,yName]
  s = as.numeric(data[,sName]) # ensures consecutive integer levels
  # Now plot the posterior:
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  Ntheta = length(grep("theta",colnames(mcmcMat)))
  openGraph(width=2.5*Ntheta,height=2.0*Ntheta)
  par( mfrow=c(Ntheta,Ntheta) )
  for ( t1Idx in 1:(Ntheta) ) {
    for ( t2Idx in (1):Ntheta ) {
      parName1 = paste0("theta[",t1Idx,"]")
      parName2 = paste0("theta[",t2Idx,"]")
      if ( t1Idx > t2Idx) {  
        # plot.new() # empty plot, advance to next
        par( mar=c(3.5,3.5,1,1) , mgp=c(2.0,0.7,0) )
        nToPlot = 700
        ptIdx = round(seq(1,chainLength,length=nToPlot))
        plot ( mcmcMat[ptIdx,parName2] , mcmcMat[ptIdx,parName1] , cex.lab=1.75 ,
               xlab=parName2 , ylab=parName1 , col="skyblue" )
      } else if ( t1Idx == t2Idx ) {
        par( mar=c(3.5,1,1,1) , mgp=c(2.0,0.7,0) )
        postInfo = plotPost( mcmcMat[,parName1] , cex.lab = 1.75 , 
                             compVal=compVal , ROPE=rope , cex.main=1.5 ,
                             xlab=parName1 , main="" )
        includeRows = ( s == t1Idx ) # identify rows of this subject in data
        dataPropor = sum(y[includeRows])/sum(includeRows) 
        points( dataPropor , 0 , pch="+" , col="red" , cex=3 )
      } else if ( t1Idx < t2Idx ) {
        par( mar=c(3.5,1,1,1) , mgp=c(2.0,0.7,0) )
        postInfo = plotPost(mcmcMat[,parName1]-mcmcMat[,parName2] , cex.lab = 1.75 , 
                           compVal=compValDiff , ROPE=ropeDiff , cex.main=1.5 ,
                           xlab=paste0(parName1,"-",parName2) , main="" )
        includeRows1 = ( s == t1Idx ) # identify rows of this subject in data
        dataPropor1 = sum(y[includeRows1])/sum(includeRows1) 
        includeRows2 = ( s == t2Idx ) # identify rows of this subject in data
        dataPropor2 = sum(y[includeRows2])/sum(includeRows2) 
        points( dataPropor1-dataPropor2 , 0 , pch="+" , col="red" , cex=3 )
      }
    }
  }
  #-----------------------------------------------------------------------------  
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName,"Post",sep=""), type=saveType)
  }
}

plotMCMC_Kappa_Omega = function( codaSamples , data , sName="s" , yName="y" , 
                     compVal=0.5 , rope=NULL , 
                     diffIdVec=NULL , compValDiff=0.0 , ropeDiff=NULL , 
                     saveName=NULL , saveType="jpg" ) {
  #-----------------------------------------------------------------------------
  # N.B.: This function expects the data to be a data frame, 
  # with one component named y being a vector of integer 0,1 values,
  # and one component named s being a factor of subject identifiers.
  y = data[,yName]
  s = as.numeric(data[,sName]) # ensures consecutive integer levels
  # Now plot the posterior:
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  # Plot omega, kappa:
  openGraph(width=3.5*2,height=3.0)
  par( mfrow=c(1,2) )
  par( mar=c(3.5,3,1,1) , mgp=c(2.0,0.7,0) )
  postInfo = plotPost( mcmcMat[,"kappa"] , compVal=NULL , ROPE=NULL ,
                       xlab=bquote(kappa) , main="" , 
                       xlim=c( min(mcmcMat[,"kappa"]),
                               quantile(mcmcMat[,"kappa"],probs=c(0.990)) ) )
  postInfo = plotPost( mcmcMat[,"omega"] , compVal=compVal , ROPE=rope ,
                       xlab=bquote(omega) , main="Group Mode" ,
                       xlim=quantile(mcmcMat[,"omega"],probs=c(0.005,0.995)) )
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName,"PostOmega",sep=""), type=saveType)
  }
  # Plot individual theta's and differences:
  if ( !is.null(diffIdVec) ) {
    Nidx = length(diffIdVec)
    openGraph(width=2.5*Nidx,height=2.0*Nidx)
    par( mfrow=c(Nidx,Nidx) )
    for ( t1Idx in 1:Nidx ) {
      for ( t2Idx in 1:Nidx ) {
        parName1 = paste0("theta[",diffIdVec[t1Idx],"]")
        parName2 = paste0("theta[",diffIdVec[t2Idx],"]")
        if ( t1Idx > t2Idx) {  
          # plot.new() # empty plot, advance to next
          par( mar=c(3.5,3.5,1,1) , mgp=c(2.0,0.7,0) , pty="s" )
          nToPlot = 700
          ptIdx = round(seq(1,chainLength,length=nToPlot))
          plot ( mcmcMat[ptIdx,parName2] , mcmcMat[ptIdx,parName1] , cex.lab=1.75 ,
                 xlab=parName2 , ylab=parName1 , col="skyblue" )
          abline(0,1,lty="dotted")
        } else if ( t1Idx == t2Idx ) {
          par( mar=c(3.5,1,1,1) , mgp=c(2.0,0.7,0) , pty="m" )
          postInfo = plotPost( mcmcMat[,parName1] , cex.lab = 1.75 , 
                               compVal=compVal , ROPE=rope , cex.main=1.5 ,
                               xlab=parName1 , main="" )
          includeRows = ( s == diffIdVec[t1Idx] ) # rows of this subject in data
          dataPropor = sum(y[includeRows])/sum(includeRows) 
          points( dataPropor , 0 , pch="+" , col="red" , cex=3 )
        } else if ( t1Idx < t2Idx ) {
          par( mar=c(3.5,1,1,1) , mgp=c(2.0,0.7,0) , pty="m" )
          postInfo = plotPost( mcmcMat[,parName1]-mcmcMat[,parName2] , 
                               compVal=compValDiff , ROPE=ropeDiff , cex.main=1.5 ,
                               xlab=paste0(parName1,"-",parName2) , main="" , 
                               cex.lab=1.75 )
          includeRows1 = ( s == diffIdVec[t1Idx] ) # rows of this subject in data
          dataPropor1 = sum(y[includeRows1])/sum(includeRows1) 
          includeRows2 = ( s == diffIdVec[t2Idx] ) # rows of this subject in data
          dataPropor2 = sum(y[includeRows2])/sum(includeRows2) 
          points( dataPropor1-dataPropor2 , 0 , pch="+" , col="red" , cex=3 )
        }
      }
    }
  }
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName,"PostTheta",sep=""), type=saveType)
  }
}

#===============================================================================
