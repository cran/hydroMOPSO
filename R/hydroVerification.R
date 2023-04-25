###############################################################################################
##                                        ReadResults                                        ##
###############################################################################################
# Author : Rodrigo Marinao Rivas                                                             ##
###############################################################################################
# Created: 2021-04-05                                                                        ##
###############################################################################################
#
# Description: Read results saved on disk from an optimization with hydroMOPSO.
#              This feature only applies when fn %in% c("hydromod", "hydromodInR")

hydroVerification <- function(Results,
                              fn = NULL, 
                              control = list(),
                              model.FUN = NULL, 
                              model.FUN.args = list()
                              ){


    if(!is.null(Results[["MOPSOResults"]])){
        MOPSOResults <- Results[["MOPSOResults"]]
    }else{
        stop("'MOPSOResults' (a result of hydroMOPSO) must be included in the 'Results' input list")
    }

    if(!is.null(Results[["hydroResults"]])){
        hydroDetails <- Results[["hydroDetails"]]
    }else{
        stop("'hydroDetails' (a result of hydroMOPSO) must be included in the 'Results' input list")
    }


    out <- PostResults(MOPSO.Results = MOPSOResults,
                       hydro.Details = hydroDetails,
                       fn = fn,
                       control = control,
                       model.FUN = model.FUN,
                       analysis.period = "verification",
                       model.FUN.args = model.FUN.args
                       )[["hydroResults"]]

    return(out)
} # END