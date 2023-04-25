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

ReadResults <- function(fn = NULL,                # >> 'character/function'. Either a character or a function indicating the function (forgive the 
                                                  # redundancy) to be optimised. When it comes to model optimisation, there are two special
                                                  # specifications: c("hydromod", "hydromodInR"), being "hydromod" proper to the use of a hydrological
                                                  # model controlled from outside R and "hydromodInR" proper to the use of a hydrological model 
                                                  # implemented within R
                        control = list(),         # >> 'list'. A list of control parameters. See 'hydroMOPSO' function in documentation for details 
                        model.FUN = NULL,         # >> 'character' (only used only when fn='hydromod' or fn='hydromodInR'). A valid R function
                                                  # representing the model code to be calibrated/optimised
                        model.FUN.args = list()   # >> 'list' (only used only when fn='hydromod' or fn='hydromodInR'). List with the arguments to pass
                                                  # to model.FUN
                        ){

    if(!is.null(control[["drty.out"]])){
        if(!dir.exists(control[["drty.out"]])){
            stop(paste0("The folder '", control[["drty.out"]],"', specified as the output directory, has not been found... check directories"))
        }else{
            out.drty <- control[["drty.out"]]
        }
    }else{
        if(!dir.exists("MOPSO.out")){
            stop(paste0("You have not specified the name you have assigned to the output directory with the results to read. By default, the name assigned in hydroMOPSO is 'MOPSO.out', but this folder has not been found... check directories"))
        }else{
            out.drty <- "MOPSO.out"
        }

    }


    if(!is.null(control[["write2disk"]])){
        if(control[["write2disk"]]){
            if(dir.exists(paste0(out.drty, "/calibration"))){
                warning(paste0("This is the situation... 
(1) You have put the control specification 'write2disk = TRUE', 
(2) You already have a folder called 'calibration' saved in your directory '", out.drty,"'  
If you proceed under this scenario there will be a rewrite of results, which in this case is not allowed... 
(If for some reason you wanted to proceed with writing results you would, at your own risk, delete the 'calibration' folder and re-run ReadResults...)
The following change has been made: 'write2disk = FALSE'
..."))
                control[["write2disk"]] <- FALSE
            }

        }
    }





    out <- PostResults(MOPSO.Results = NULL,
                       hydro.Details = NULL,
                       fn = fn,
                       control = control,
                       model.FUN = model.FUN,
                       analysis.period = "calibration",
                       model.FUN.args = model.FUN.args
                       )

    return(out)
} # END