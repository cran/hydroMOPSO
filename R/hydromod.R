# File hydromod.R
# Part of the hydroPSO R package and hydroMOPSO R package
# Copyright 2010-2022 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
# Distributed under GPL 2 or later

################################################################################
#                           hydromod                                           #
################################################################################
# Purpose    : To run a hydrological/environmental model, and get as many      #
#              otputs as indicated                                             #
################################################################################
# Output     : A list with as many eaoutput variables (time series usually)    #
#              as functions listed in out.FUNs                                 #
#                                                                              #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Colab  : Rodrigo Marinao-Rivas
# Started: 14-Dec-2010 at JRC Ispra                                            #
# Updates: 20-Dec-2010                                                         #
#          09-Mar-2020                                                         #
#          12-Abr-2021 by RMR at El Rancho                                     #
#                                                                              #
################################################################################
hydromod <- function(
  param.values,                 # >> 'numeric'. Vector with the parameter values that will be used in the input files of the hydrological model \code{exe.fname}
  param.files="ParamFiles.txt", # >> 'character'. Name of the file (with full path) that stores the name of the files that have to be modified for each parameter 
  model.drty=getwd(),           # >> 'character'. Path of the directory that stores the exe file of the hydrological model
  exe.fname,                    # >> 'character'. File name of the external executable or script that runs de hydrological model. Note that for executables in
                                # Linux OS you should prepend the string "./" (e.g., './swat.sh' instead of just 'swat.sh')
  exe.args = character(),       # >> 'character' (if required). With optional arguments to be passed in the command line to the user-defined model.
  stdout=FALSE,                 # >> 'logical/character' (with options c("",TRUE,FALSE)). Logical indicating whether messages written to 'stdout' should be sent or not. See '?system2'
  stderr="",                    # >> 'logical/character' (with options c("",TRUE,FALSE)). Logical indicating whether messages written to 'stderr' should be sent or not. See '?system2'
  verbose= FALSE,               # >> 'logical'.  Logical indicating if progress messages have to be printed during the simulations. If \code{verbose=TRUE}, the 
                                # following messages will appear: i)parameter values for each particle; (ii) model execution; iii) extraction of simulated 
                                # values; and iv) computation of the goodness-of-fit measure.
  out.FUNs,                     # >> 'character'. Vector or single character specifying as many valid R functions (just names) as outputs to read. Each name 
                                # represent the function to be used for reading the respective outputs in 'out.fname' and transforming them into a zoo object
  out.FUNs.args                 # >> 'list'. List with as many lists ((sub)lists) as outputs to read. Each (sub)list contain the arguments to be passed to the 
                                # respective 'out.FUN' (additional to 'sim' and 'obs')
  # ...                         # (discarded) Additional arguments to be passed to                    
  
) {
  
  ##############################################################################
  # 0)                             Checkings                                   #
  ##############################################################################
  
  # Verifying 'param.values'
  #if (missing(param.values))
  #  stop( "Missing argument: 'param.values' has to be given !")
  
  if (!file.exists(param.files))
    stop( "Invalid argument: the file '", param.files, "' doesn't exist!" )
  
  if ( missing(exe.fname) )
    stop( "Missing argument: 'exe.fname'" )
  
  if ( missing(out.FUNs) ) {
    stop( "Missing argument: 'out.FUNs'" )
  }# else out.FUN <- match.fun(out.FUNs)
  
  oldwd <- getwd()
  on.exit(setwd(oldwd))

  setwd(model.drty)

  
  if (!file.exists(exe.fname))
    stop( "Invalid argument: the file '", exe.fname, "' does not exist!" )
  
  if ( sessionInfo()[[1]]$os != "linux-gnu") {
    dot.pos   <- which(strsplit(exe.fname, split=character(0))[[1]] == ".")
    ext       <- substr(exe.fname,start=dot.pos+1, stop=nchar(exe.fname)) 
    if (ext=="exe") exe.fname <- substr(exe.fname,start=1, stop=dot.pos-1) 
  } # IF end
  
  ##############################################################################  
  # 1)     New parameter values -> input files of the hydrological model       #
  ##############################################################################  
  if (verbose) message("                                           ")
  if (verbose) message("===========================================")
  if (verbose) message("[ 1) Writing new parameter values ...     ]")
  if (verbose) message("===========================================")
  ParameterValues2InputFiles(NewValues=param.values, ParamFiles.fname=param.files,
                             verbose=FALSE)
  
  
  ##############################################################################
  # 2)                       Running the hydrological model                    #
  ##############################################################################
  
  if (verbose) message("===========================================")
  if (verbose) message("[ 2) Running the model ...                ]")
  if (verbose) message("===========================================")
  system2(exe.fname, args = exe.args, stdout=stdout, stderr=stderr)
  
  ##############################################################################
  #                        Extracting simulated values                         #
  #                                   and
  #                        Creating the output object                          #                                 
  ##############################################################################
  ##############################################################################
  
  if (verbose) message("===========================================")
  if (verbose) message("[ 3) Extracting simulated values ...      ]")
  if (verbose) message("===========================================")
  
  out <- vector(mode = "list", length = length(out.FUNs)) 
  
  for(i in 1:length(out)){
    out.FUNs.argsDefaults <- formals(out.FUNs[i])
    out.FUNs.args.mod     <- modifyList(out.FUNs.argsDefaults, out.FUNs.args[[i]]) 
    out[[i]]              <- do.call(out.FUNs[i], as.list(out.FUNs.args.mod))   
  }
  
  return(out)
  
} # 'hydromod' END