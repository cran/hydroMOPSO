################################################################################
##                                 GR4JExampleCal                             ##
################################################################################
# Author : Rodrigo Marinao Rivas                                              ##
################################################################################
# Created: 2022-04-04                                                         ##
################################################################################
# Main references
#
#
# Description: Example wrapper function to execute the GR4J model and obtain the
#              performance of two objective functions (KGE2012 and KGEGarcia)
#              in a CALIBRATION period
#
# Function and Input arguments
# Important: All the wrapper functions to be used in hydroMOPSO will require mandatory specifications:
# >> Mandatory inputs: Eight input arguments must be identical to those presented in the following example, both in class and name
# >> Mandatory outputs: The function must return a list, with two outputs that must be identical to those presented in the following example, both in class and name
GR4JExampleCal <- function(# Mandatory Inputs below
                           param.values, # >> 'numeric'. Vector with parameter set of the model
                           Obs, # >> 'list'. List with time series of observations of the output variables
                           Objs.names, # >> 'character'. Vector with the names of the optimisation objectives
                           var.names, # >> 'character'. Vector with the names of the output variables
                           var.units, # >> 'character'. Vector with the units of measurement of the output variables
                           full.period, # >> 'Date'. Vector with the dates of the full period (warmup + calibration and/or verification)
                           warmup.period, # >> 'Date'. Vector with the dates of the warmup period
                           cal.period, # >> 'Date'. Vector with the dates of the calibration period
                           # Model Specific Inputs below (in this case, from GR4J)
                           InputsModel, # >> 'list'. GR4J inputs structured with the function airGR::CreateInputsModel
                           RunOptions, # >> 'list'. GR4J run options specified with the function airGR::CreateRunOptions
                           area # >> 'numeric.'Area of the basin (sq-m), necessary to pass the outlet streamflow to m3/s (cms)
                           ){

  simLump <- airGR::RunModel(Param=param.values, InputsModel=InputsModel, RunOptions=RunOptions, FUN_MOD=airGR::RunModel_GR4J) # output of GR4J

  qsim.model <- zoo(as.numeric(simLump$Qsim)*area/(1000*24*60*60), full.period) # streamflow to zoo time serie

  qsim <- qsim.model # raw simulated streamflow
  qobs <- Obs[[1]] # raw observed streamflow

  na.index <- rep(FALSE, length(full.period)) # initialises a vector that will help identify the warmup period
  na.index[full.period %in% warmup.period] <- TRUE # identification of the warmup period

  qsim[na.index] <- NA # on the dates specified as warmup period the data is redefined as NA
  qobs[na.index] <- NA # on the dates specified as warmup period the data is redefined as NA

  qmean100 <- mean(qobs, na.rm = TRUE)/100 # 1%Qmean: this is required in the calculation of KGEGarcia


  # It is verified that in the CALIBRATION period the model has not delivered NA, otherwise the performance calculation is invalidated
  if(sum(na.index) == sum(is.na(qsim))){ 

    gof1 <- hydroGOF::KGE(sim=qsim, obs=qobs, method = "2012") # KGE2012 (Kling et al., 2012)
    gof2 <- 0.5*hydroGOF::KGE(sim=1/(qsim+qmean100), obs=1/(qobs+qmean100), method = "2012") + 0.5*hydroGOF::KGE(sim=qsim, obs=qobs, method = "2012") # KGEGarcia (Garcia et al., 2017)

  }else{

    gof1 <- NA
    gof2 <- NA

  }
  
  # Initialisation of the mandatory output (attention to classes and names)
  out<- vector("list", 2) 
  names(out) <- c("Objs", "sim") 

  out[[1]]<- c(gof1, gof2)
  out[[2]]<- list(qsim.model)
  
  return(out)
  
} # 'GR4JExampleCal' end




################################################################################
##                                 GR4JExampleVer                             ##
################################################################################
# Author : Rodrigo Marinao Rivas                                              ##
################################################################################
# Created: 2022-04-04                                                         ##
################################################################################
# Main references
#
#
# Description: Example wrapper function to execute the GR4J model and obtain the
#              performance of two objective functions (KGE2012 and KGEGarcia),
#              in a VERIFICATION period
#
# Function and Input arguments
# Important: All the wrapper functions to be used in hydroMOPSO will require mandatory specifications:
# >> Mandatory inputs: Eight input arguments must be identical to those presented in the following example, both in class and name
# >> Mandatory outputs: The function must return a list, with two outputs that must be identical to those presented in the following example, both in class and name


GR4JExampleVer <- function(# Mandatory Inputs below
                           param.values, # >> 'numeric'. Vector with parameter set of the model
                           Obs, # >> 'list'. List with time series of observations of the output variables
                           Objs.names, # >> 'character'. Vector with the names of the optimisation objectives
                           var.names, # >> 'character'. Vector with the names of the output variables
                           var.units, # >> 'character'. Vector with the units of measurement of the output variables
                           full.period, # >> 'Date'. Vector with the dates of the full period (warmup + calibration and/or verification)
                           warmup.period, # >> 'Date'. Vector with the dates of the warmup period
                           cal.period, # >> 'Date'. Vector with the dates of the calibration period
                           # Model Specific Inputs below (in this case, from GR4J)
                           InputsModel, # >> 'list'. GR4J inputs structured with the function airGR::CreateInputsModel
                           RunOptions, # >> 'list'. GR4J run options specified with the function airGR::CreateRunOptions
                           area # >> 'numeric.'Area of the basin (sq-m), necessary to pass the outlet streamflow to m3/s (cms)
                           ){

  simLump <- airGR::RunModel(Param=param.values, InputsModel=InputsModel, RunOptions=RunOptions, FUN_MOD=airGR::RunModel_GR4J)  #raw output of GR4J

  qsim.model <- zoo(as.numeric(simLump$Qsim)*area/(1000*24*60*60), full.period) # streamflow to zoo time serie

  qsim <- qsim.model # raw simulated streamflow
  qobs <- Obs[[1]] # raw observed streamflow

  na.index <- rep(FALSE, length(full.period)) # initialises a vector that will help identify the warmup and calibration period
  na.index[full.period %in% warmup.period] <- TRUE # identification of the warmup period
  na.index[full.period %in% cal.period] <- TRUE # identification of the calibration period

  qsim[na.index] <- NA # on the dates specified as warmup and calibration period the data is redefined as NA
  qobs[na.index] <- NA # on the dates specified as warmup and calibration period the data is redefined as NA

  qmean100 <- mean(qobs, na.rm = TRUE)/100 # 1%Qmean: this is required in the calculation of KGEGarcia

  # It is verified that in the VERIFICATION period the model has not delivered NA, otherwise the performance calculation is invalidated
  if(sum(na.index) == sum(is.na(qsim))){

    gof1 <- hydroGOF::KGE(sim=qsim, obs=qobs, method = "2012") # KGE2012 (Kling et al., 2012)
    gof2 <- 0.5*hydroGOF::KGE(sim=1/(qsim+qmean100), obs=1/(qobs+qmean100), method = "2012") + 0.5*hydroGOF::KGE(sim=qsim, obs=qobs, method = "2012") # KGEGarcia (Garcia et al., 2017)

  }else{

    gof1 <- NA
    gof2 <- NA
    
  }
  
  # Initialisation of the mandatory output (attention to classes and names)
  out<- vector("list", 2) 
  names(out) <- c("Objs", "sim") 

  out[[1]]<- c(gof1, gof2)
  out[[2]]<- list(qsim.model)
  
  return(out)
  
} # 'GR4JExampleVer' end