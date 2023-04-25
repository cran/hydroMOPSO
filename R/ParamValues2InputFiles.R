# Part of the hydroPSO package, http://www.rforge.net/hydroPSO/
# Copyright 2010-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                       ParameterValues2InputFiles                             #
################################################################################
# Purpose    : To write several values into several plain text files, by using #
#              the filenames, row and column positions defined in the          #
#              'ParamFiles.fname' file                                         #
################################################################################
# Output     : A mofified text file ('filename')                               #
################################################################################
# Author     : Mauricio Zambrano-Bigiarini                                     #
# Started    : 15-Dec-2010 at JRC Ispra                                        #
# Updates    : 12-May-2011                                                     #
################################################################################
ParameterValues2InputFiles <- function(NewValues,
                                       ParamFiles.fname="ParamFiles.txt",
                                       verbose=TRUE
                                       ) {

  # Checking 'NewValues'
  if ( is.na( match(class(NewValues), c("numeric", "integer") ) ) )
    stop("Invalid argument: class(NewValues) has to be in c('numeric', 'integer')") 
  
  # Number of values provided by the user
  nval <- length(NewValues)
  
  # Reading the file with the location of the paramters
  ParamFiles <-  ReadParamFile(file=ParamFiles.fname)
  
  n1 <- length(levels(as.factor(ParamFiles[,1]))) # Number of Param IDs
  n2 <- length(levels(as.factor(ParamFiles[,2]))) # Number of Param Names
  if ( n1 != n2)
    stop( paste("In '", ParamFiles.fname, "' : Number of 'ParameterNmbr' != Number of 'ParameterName' (", n1, " != ", n2, ")", sep="" ) ) 
  if ( n1 != nval)
    stop( paste("Number of Parameters != Number of Values' (", n1, " != ", nval, ")", sep="" ) )
       
  # Number of files that have to be changed
  nfiles <- nrow(ParamFiles)

  # Loop in all the files that have to be changed
  for (i in 1:nfiles) {

    ParamID    <- ParamFiles[i,1]
    ParamName  <- ParamFiles[i,2]
    filename   <- ParamFiles[i,3]
    lrow       <- ParamFiles[i,4]
    col.ini    <- ParamFiles[i,5]
    col.fin    <- ParamFiles[i,6]
    decimals   <- ParamFiles[i,7]
    
    if(ncol(ParamFiles) >= 11){
      TypeChange <- ParamFiles[i,8] # character, specification of the type of parameter modification ("repl", "mult", "addi")
      RefValue   <- ParamFiles[i,9] # numeric, only used when TypeChange == "mult" |  TypeChange == "addi", reference value for making de parameter modification
      MinValue   <- ParamFiles[i,10]
      MaxValue   <- ParamFiles[i,11]
    }else{
      TypeChange <- "repl"
      RefValue   <- 0
      MinValue   <- 0
      MaxValue   <- 0
      
    }
    
    ModifyInputFile(ParamID=ParamName, newvalue= NewValues[ParamID], 
                    TypeChange = TypeChange, RefValue = RefValue, MinValue = MinValue, MaxValue = MaxValue,
                    filename=filename, row=lrow, col.ini= col.ini, col.fin=col.fin, 
                    decimals=decimals, verbose=verbose)
 
  } # FOR end
              
  
} # 'ParameterValues2InputFiles' end


# Part of the hydroPSO package, http://www.rforge.net/hydroPSO/
# Copyright 2010-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                            SpecificValueInFile                               #
################################################################################
# Purpose    : Straightforward modification of a value in file                 #
################################################################################
# Output     : A modified text file ('filename')                               #
################################################################################
# Author     : Rodrigo                                                         #
# Started    : 21-Feb-2022 at 'El Rancho' in Freire                            #
# Updates    :                                                                 #
################################################################################


SpecificValueInFile <- function(modlist){
  for(i in 1:length(modlist)){
    ModifyInputFile(ParamID = modlist[[i]][["ParamID"]],
                    newvalue = modlist[[i]][["newvalue"]],
                    TypeChange = "repl",
                    RefValue = NA,
                    MinValue = NA, 
                    MaxValue = NA,
                    filename = modlist[[i]][["filename"]],
                    row = modlist[[i]][["row"]],
                    col.ini = modlist[[i]][["col.ini"]],
                    col.fin = modlist[[i]][["col.fin"]],
                    decimals = modlist[[i]][["decimals"]],
                    verbose=TRUE)
  }
}

