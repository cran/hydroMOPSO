###############################################################################################
##                                        hydroMOPSO                                         ##
###############################################################################################
# Author : Rodrigo Marinao Rivas                                                             ##
###############################################################################################
# Created: during 2020                                                                       ##
###############################################################################################
# Main references: Lin et al. (2018); Marinao-Rivas & Zambrano-Bigiarini (2021)
#
# Description: Multi-objective Particle Swarm Optimisation algorithm (NMPSO). The default 
# configuration of hydroMOPSO has been adapted to obtain results with the fewest number of 
# iterations possible.
#
# Function and Input arguments
hydroMOPSO <-function(fn="hydromod",         # >> 'character/function'. Either a character or a function indicating the function (forgive the 
                                             # redundancy) to be optimised. When it comes to model optimisation, there are two special
                                             # specifications: c("hydromod", "hydromodInR"), being "hydromod" proper to the use of a hydrological
                                             # model controlled from outside R and "hydromodInR" proper to the use of a hydrological model 
                                             # implemented within R.
                      lower=-Inf,            # >> 'numeric'. Vector with minimum possible value for each parameter
                      upper=Inf,             # >> 'numeric'. Vector with maximum possible value for each parameter             
                      control=list(),        # >> 'list'. A list of control parameters. See 'hydroMOPSO' function in documentation for details 
                      model.FUN=NULL,        # >> 'character' (only used only when fn='hydromod' or fn='hydromodInR'). A valid R function
                                             # representing the model code to be calibrated/optimised
                      model.FUN.args=list(), # >> 'list' (only used only when fn='hydromod' or fn='hydromodInR'). List with the arguments to pass
                                             # to model.FUN
                      obj.thr = NULL,
                      ...                    # Further arguments to pass to fn, if needed
                      ){ 

  
  if (missing(fn)){
    
    stop("Missing argument: 'fn' must be provided")
    
  }else if (is.character(fn) | is.function(fn)) {
    
    if (is.character(fn)) {
      
      if (fn == "hydromod") {
        fn.name <- fn
        #fn <- match.fun(fn)
      }
      else if (fn == "hydromodInR") {
        fn.name <- fn
        fn <- match.fun(model.FUN)
      }
      else stop("Invalid argument: valid character values for 'fn' are only: c('hydromod', 'hydromodInR')")
    }
    else if (is.function(fn)) {
      fn.name <- as.character(substitute(fn))
      fn <- fn
    }
  }else stop("Missing argument: 'class(fn)' must be in c('function', 'character')")
  
  
  if (length(lower) != length(upper)){
    stop("Invalid argument: 'length(lower) != length(upper) (", length(lower), "!=", length(upper), ")'")
  }

  n <- length(lower)

  lapplyNA <- function(x) sapply(lapply(x, FUN = is.na), FUN = any)

  
  con <- list(drty.in = "MOPSO.in",
              drty.out = "MOPSO.out", 
              param.ranges = "ParamRanges.txt", 
              digits = 8,
              MinMax = c("min", "max"),
              npart = 10, 
              maxit = 1000,
              maxeval = 40000,
              maxrep = 100,
              maxcross = 50,
              digits.dom = Inf,
              cal.hv = FALSE,
              nadir.point = NULL,
              samples = 10000,
              Xini.type = c("Sobol", "lhs"),
              Vini.type = "zero",
              boundary.wall = "absorbing2011",
              write2disk=FALSE,
              verbose = TRUE,
              plot = FALSE,
              REPORT = 1,
              parallel = c("none", "parallel", "parallelWin"),
              par.nnodes = NA,
              par.pkgs = NULL
  )
  MinMax <- match.arg(control[["MinMax"]], con[["MinMax"]])
  Xini.type <- match.arg(control[["Xini.type"]], con[["Xini.type"]])
  Vini.type <- match.arg(control[["Vini.type"]], con[["Vini.type"]])
  Vini.type <- if (is.na(Vini.type)) {"random2011"} else Vini.type
  boundary.wall <- match.arg(control[["boundary.wall"]], con[["boundary.wall"]])
  parallel <- match.arg(control[["parallel"]], con[["parallel"]])
  nmsC <- names(con)
  con[(namc <- names(control))] <- control
  
  cal.hv <- con[["cal.hv"]]
  nadir.point <- con[["nadir.point"]]
  
  if(cal.hv){
    if(is.numeric(con[["samples"]])){
      abs(floor(con[["samples"]]))
    }else{
      samples <- 10000
    }
  }else{
    samples <- NULL
  }
  
  
  if (length(noNms <- namc[!namc %in% nmsC])){
    warning("[Unknown names in control: ", paste(noNms, collapse = ", "), " (not used) !]")
  }

  
  drty.in <- con[["drty.in"]]
  drty.out <- con[["drty.out"]]
  param.ranges <- con[["param.ranges"]]
  digits <- con[["digits"]]
  npart <- con[["npart"]]
  maxit <- con[["maxit"]]
  maxeval <- con[["maxeval"]]
  maxrep <- con[["maxrep"]]
  maxcross <- con[["maxcross"]]
  digits.dom <- con[["digits.dom"]]
  write2disk <- as.logical(con[["write2disk"]])
  verbose <- as.logical(con[["verbose"]])
  plot <- as.logical(con[["plot"]])
  REPORT <- con[["REPORT"]]
  par.nnodes <- con[["par.nnodes"]]
  par.pkgs <- con[["par.pkgs"]]
  if (maxit < REPORT) {
    REPORT <- maxit
    warning("[ 'REPORT' is greater than 'maxit' => 'REPORT=maxit' ]")
  }
  
  
  if(cal.hv){
    df.hv <- data.frame(iter = seq(1,maxit), neval = seq(1,maxit), HV = rep(NA, maxit))
  }
  
  
  if (verbose) message("                                                                                ")
  if (verbose) message("================================================================================")
  if (verbose) message("[                                Initialising  ...                             ]")
  if (verbose) message("================================================================================")
  if (verbose) message("                                                                                ")
  
  tmp.stg <- c("npart", "maxit", "boundary.wall")
  tmp.val <- c(npart, maxit, boundary.wall)
  message("[", paste(tmp.stg, tmp.val, collapse = " ; ", sep = "="), "]")
  if (length(yesNms <- namc[namc %in% nmsC])){
    yesVals <- con[pmatch(namc[namc %in% nmsC], nmsC)][c(yesNms)]
    message("         ")
    message("[ user-definitions in control: ", paste(yesNms, yesVals, collapse = " ; ", sep = "="), " ]")
    message("         ")
  }
  
  
  if (fn.name == "hydromod") {
    if (drty.in == basename(drty.in)) 
      drty.in <- paste(getwd(), "/", drty.in, sep = "")
    if (!file.exists(file.path(drty.in))) 
      stop("Invalid argument: The directory '", drty.in, "' doesn't exist !")
    if (param.ranges == basename(param.ranges)) 
      param.ranges <- paste(file.path(drty.in), "/", param.ranges, sep = "")
    if (!file.exists(param.ranges)) 
      stop("Invalid argument: The file '", param.ranges, "' doesn't exist !")
    if (is.null(model.FUN)){
      stop("'model.FUN' has to be defined !")
    }
    else {
      model.FUN.name <- model.FUN
      model.FUN <- match.fun(model.FUN)
    }
    
    if (length(model.FUN.args) == 0) {
      warning("['model.FUN.args' is an empty list. Are you sure your model does not have any argument(s) ?]")
    }
    else {
      model.FUN.argsDefaults <- formals(model.FUN)
      model.FUN.args <- modifyList(model.FUN.argsDefaults, model.FUN.args)
    }
  }
  
  if (fn.name == "hydromodInR") {
    if (is.null(model.FUN)) {
      stop("'model.FUN' has to be defined !")
    }
    else {
      model.FUN.name <- as.character(substitute(model.FUN))
      model.FUN <- match.fun(model.FUN)
    }
    if (!("param.values" %in% names(formals(model.FUN)))) 
      stop("[ Invalid argument: 'param.values' must be the first argument of the 'model.FUN' function! ]")
    # if (!("obs" %in% names(formals(model.FUN)))) 
    #   stop("[ Invalid argument: 'obs' must be an argument of the 'model.FUN' function! ]")
    model.FUN.argsDefaults <- formals(model.FUN)
    if (length(model.FUN.args) > 0) {
      model.FUN.args <- modifyList(model.FUN.argsDefaults, model.FUN.args)
      formals(model.FUN) <- model.FUN.args
    }else{
      model.FUN.args <- model.FUN.argsDefaults
    }
  }
  
  
  
  if (fn.name == "hydromod") {
    if (verbose) 
      message("================================================================================")
    if (verbose) 
      message("[                          Reading 'param.ranges' ...                          ]")
    if (verbose) 
      message("================================================================================")
    X.Boundaries <- ReadParamRanges(ParamRanges.fname = param.ranges)
    lower <- X.Boundaries[, 1]
    upper <- X.Boundaries[, 2]
  }else {
    if ((lower[1L] == -Inf) || (upper[1L] == Inf)) {
      stop("Invalid argument: 'lower' and 'upper' boundaries must be finite !!'")
    }
    else X.Boundaries <- cbind(lower, upper)
  }
  
  n <- nrow(X.Boundaries)
  
  if (is.null(rownames(X.Boundaries))) {
    param.IDs <- paste("Param", 1:n, sep = "")
  }else{
    param.IDs <- rownames(X.Boundaries)
  }
  
  if (drty.out == basename(drty.out)){
    drty.out <- paste(getwd(), "/", drty.out, sep = "")
  }
  
  if (!file.exists(file.path(drty.out))) {
    if (write2disk) {
      dir.create(file.path(drty.out))
      if (verbose) message("                                            ")
      if (verbose) message("[ Output directory '", basename(drty.out), "' was created on: '", dirname(drty.out), "' ]") 
      if (verbose) message("                                            ")
    } # IF end
  } # IF end 
  
  if(Xini.type == "lhs"){
    if (length(find.package("lhs", quiet = TRUE)) == 0) {
      warning("[ Package 'lhs' is not installed =>  Xini.type='random' ]")
      Xini.type <- "random"
    }
  }
  
  if(Xini.type == "Sobol"){
    if (length(find.package("randtoolbox", quiet = TRUE)) == 0) {
      warning("[ Package 'randtoolbox' (for Sobol initialisation) is not installed =>  Xini.type='random' ]")
      Xini.type <- "random"
    }
  }
  
  if (Vini.type %in% c("lhs2011", "lhs2007")) {
    if (length(find.package("lhs", quiet = TRUE)) == 0) {
      warning("[ Package 'lhs' is not installed =>  Vini.type='random2011' ]")
      Vini.type <- "random2011"
    }
  }
  
  
  
  
  Lmax <- (X.Boundaries[, 2] - X.Boundaries[, 1])
  if (parallel != "none"){
    
    ifelse(parallel == "parallelWin", parallel.pkg <- "parallel", parallel.pkg <- parallel)
    
    if (length(find.package(parallel.pkg, quiet = TRUE)) == 0) {
      warning("[ Package '", parallel.pkg, "' is not installed =>  parallel='none' ]")
      parallel <- "none"
    }else{
      if (verbose) message("                               ")
      if (verbose) message("[ Parallel initialisation ... ]")
      
      fn1 <- function(i, x){
        fn(x[i, ])
      }
      
      nnodes.pc <- parallel::detectCores()
      if(verbose)  message("[ Number of cores/nodes detected: ",  nnodes.pc, " ]")
      
      if((parallel == "parallel") | (parallel == "parallelWin")){
        logfile.fname <- paste(file.path(drty.out), "/", "parallel_logfile.txt", sep = "")
        if(file.exists(logfile.fname)){
          file.remove(logfile.fname)
        }
      }
      
      if (is.na(par.nnodes)){
        par.nnodes <- nnodes.pc
      }else if(par.nnodes > nnodes.pc){
        warning("[ 'nnodes' > number of detected cores (",  par.nnodes, ">", nnodes.pc, ") =>  par.nnodes=", nnodes.pc, " ] !")
        par.nnodes <- nnodes.pc
      }
      
      max.neval.periter <- max(npart, maxcross)
      
      if (par.nnodes > max.neval.periter) {
        warning("[ 'par.nnodes' > npart (", par.nnodes, ">", max.neval.periter, ") =>  par.nnodes=", max.neval.periter, " ] !")
        par.nnodes <- max.neval.periter
      }
      
      if (verbose) {
        message("[ Number of cores/nodes used    : ",  par.nnodes, " ]")
      }
      if(parallel == "parallel"){
        ifelse(write2disk, 
               cl <- parallel::makeForkCluster(nnodes = par.nnodes, outfile=logfile.fname),
               cl <- parallel::makeForkCluster(nnodes = par.nnodes) )

        on.exit(parallel::stopCluster(cl))
        
      }else if (parallel == "parallelWin"){
        ifelse(write2disk,
               cl <- parallel::makePSOCKcluster(names = par.nnodes, outfile=logfile.fname),
               cl <- parallel::makePSOCKcluster(names = par.nnodes) )

        on.exit(parallel::stopCluster(cl))
        
        pckgFn <- function(packages){
          for (i in packages) library(i, character.only = TRUE)
        }
        
        parallel::clusterCall(cl, pckgFn, par.pkgs)
        parallel::clusterExport(cl, ls.str(mode = "function", envir = .GlobalEnv))
        
        if ( (fn.name=="hydromod") | (fn.name == "hydromodInR") ) {
          fn.default.vars <- as.character(formals(model.FUN))
          parallel::clusterExport(cl, fn.default.vars[fn.default.vars %in% ls(.GlobalEnv)])
          #parallel::clusterExport(cl, model.FUN.args$out.FUN)
          #parallel::clusterExport(cl, model.FUN.args$gof.FUN)
        } # IF end
        
        #if (fn.name == "hydromodInR") {
          #fn.default.vars <- as.character(formals(model.FUN))
          #parallel::clusterExport(cl, fn.default.vars[fn.default.vars %in% ls(.GlobalEnv)])
        #} # IF end       
        
      }
      if (fn.name=="hydromod") {
        
        
        if (!("model.drty" %in% names(formals(hydromod)) )) {
          stop("[ Invalid argument: 'model.drty' has to be an argument of the 'hydromod' function! ]")
        } else { # Copying the files in 'model.drty' as many times as the number of cores
          
          model.drty <- path.expand(model.FUN.args$model.drty)
    
          files <- list.files(model.drty, full.names=TRUE, include.dirs=TRUE) 
          tmp <- which(basename(files)=="parallel")
          if (length(tmp) > 0) files <- files[-tmp]
          parallel.drty <- paste(file.path(model.drty), "/parallel", sep="")
          
          if (file.exists(parallel.drty)) {                      
            if (verbose) message("[ Removing the 'parallel' directory ... ]")    
            try(unlink(parallel.drty, recursive=TRUE, force=TRUE))
          } # IF end 
          dir.create(parallel.drty)
          
          mc.dirs <- character(par.nnodes)
          if (verbose) message("                                                     ")
          for (i in 1:par.nnodes) {
            mc.dirs[i] <- paste(parallel.drty, "/", i, "/", sep="")
            dir.create(mc.dirs[i])
            if (verbose) message("[ Copying model input files to directory '", mc.dirs[i], "' ... ]")
            file.copy(from=files, to=mc.dirs[i], overwrite=TRUE, recursive=TRUE)
          } # FOR end
          
          tmp       <- ceiling(max.neval.periter/par.nnodes)        
          part.dirs <- rep(mc.dirs, tmp)[1:max.neval.periter]  
        } # ELSE end                 
      } # IF end
    }
  }
  
  
  Vmax <- Lmax
  X <- InitializateX(npart = npart, x.MinMax = X.Boundaries, x.ini.type = Xini.type)
  V <- InitializateV(npart = npart, x.MinMax = X.Boundaries, v.ini.type = Vini.type, Xini = X)
  V <- matrix(apply(V, MARGIN = 1, FUN = VelocityBoundaryTreatment, vmax = Vmax), nrow = npart, byrow = TRUE)
  
  colnames(X) <- param.IDs
  colnames(V) <- param.IDs
  

  
  iter <- 1
  
  if(MinMax == "max"){
    sign <- -1
  }else{
    sign <- 1
  }
  
  ##
  Pop <- vector("list", 6)
  names(Pop) <- c("Part", "Position", "Velocity", "Objs", "Best.Position", "Best.Objs")
  Pop[["Part"]] <- seq(1, npart)
  ##
  
  end.type.stg <- "Unknown"
  end.type.code <- "-999"
  if (verbose) message("                                                                                ")
  if (verbose) message("================================================================================")
  if (verbose) message("[                                Running  NMPSO ...                            ]")
  if (verbose) message("================================================================================")
  if (verbose) message("                                                                                ")
  
  cont_eval <- 0
  
  while ((iter <= maxit)) {
    
    #================================================================================================
    #============================= PRINCIPAL ITERATION: NMPSO ALGORITHM =============================
    #================================================================================================
    
    if ( (fn.name == "hydromod") | (fn.name == "hydromodInR") ) {
      ModelOutPop <- vector("list", npart)
    }
    
    
    Pop[["Position"]] <- X
    Pop[["Velocity"]] <- V
    
    Xn <- X
    Vn <- V
    
    cont_eval <- cont_eval + nrow(Pop[["Position"]])
    
    #Running de model
    
    if ( (fn.name != "hydromod") & (fn.name != "hydromodInR") ) {
      
      # Evaluating an R Function -------------------------------------------------
      # --------------------------------------------------------------------------
      if (parallel=="none") {
        out <- apply(Xn, fn, MARGIN=1, ...)
      } else if ( (parallel=="parallel") | (parallel=="parallelWin") ) {
        out <- parallel::parRapply(cl= cl, x=Xn, FUN=fn, ...)
      } 
      
      
      ############
      ########
      ####
      
      ObjsProv <- matrix(NA, nrow = npart, ncol = length(out[[1]][["Objs"]]))
      
      for (part in 1:npart) {
        
        if(any(is.na(out[[part]][["Objs"]]))){
          
          if(iter == 1){
            ObjsProv[part,] <- out[[part]][["Objs"]]
          }else{
            Pop[["Position"]][part,] <- Pop.Saved[["Position"]][part,]
            ObjsProv[part,] <- Pop.Saved[["Objs"]][part,]
          }
          
        }else{
          
          ObjsProv[part,] <- out[[part]][["Objs"]]
          
        }
        
      }
      
      Pop[["Objs"]] <- ObjsProv
      
      ####
      #######
      ##########
      
      #---------------------------------------------------------------------------
      # --------------------------------------------------------------------------
      
      
      
    } else if (fn.name == "hydromod") {
      
      # Evaluating an hydromod Function ------------------------------------------
      # --------------------------------------------------------------------------
      
      if ("verbose" %in% names(model.FUN.args)) {
        verbose.FUN <- model.FUN.args[["verbose"]]
      }
      else verbose.FUN <- verbose
      if (parallel == "none") {
        
        
        out <- lapply(1:npart, 
                      hydromodEval, 
                      Particles = Xn,
                      iter = iter, 
                      npart = npart, 
                      maxit = maxit, 
                      REPORT = REPORT, 
                      verbose = verbose.FUN, 
                      digits = digits, 
                      model.FUN = model.FUN,
                      model.FUN.args = model.FUN.args,
                      parallel = parallel, 
                      ncores = par.nnodes, 
                      part.dirs = mc.dirs)
      }
      else if ((parallel == "parallel") | (parallel == "parallelWin")) {
        out <- parallel::clusterApply(cl = cl, 
                                      x = 1:npart,
                                      fun = hydromodEval, 
                                      Particles = Xn, 
                                      iter = iter,
                                      npart = npart, 
                                      maxit = maxit,
                                      REPORT = REPORT, 
                                      verbose = verbose.FUN, 
                                      digits = digits,
                                      model.FUN = model.FUN, 
                                      model.FUN.args = model.FUN.args,
                                      parallel = parallel, 
                                      ncores = par.nnodes,
                                      part.dirs = part.dirs)
      }
      
      ############
      ########
      ####
      
      ObjsProv <- matrix(NA, nrow = npart, ncol = length(out[[1]][["Objs"]]))
      
      for (part in 1:npart) {
        
        if(any(is.na(out[[part]][["sim"]]))){
          
          if(iter == 1){
            ObjsProv[part,] <- out[[part]][["Objs"]]###############
            ModelOutPop[[part]] <- out[[part]][["sim"]]##################
          }else{
            Pop[["Position"]][part,] <- Pop.Saved[["Position"]][part,]
            ObjsProv[part,] <- Pop.Saved[["Objs"]][part,]
            ModelOutPop[[part]] <- ModelOutPop.Saved[[part]]
          }
        }else{
          
          ObjsProv[part,] <- out[[part]][["Objs"]]###############
          ModelOutPop[[part]] <- out[[part]][["sim"]]##################
          
        }
        
      }
      
      Pop[["Objs"]] <- ObjsProv
      
      ####
      #######
      ##########
      
      #---------------------------------------------------------------------------
      # --------------------------------------------------------------------------
      
      
      
    }else if (fn.name == "hydromodInR") {
      
      # Evaluating an hydromodInR Function ---------------------------------------
      # --------------------------------------------------------------------------
      
      if (parallel == "none") {
        out <- apply(Xn, model.FUN, MARGIN = 1, ...)
      }
      else if ((parallel == "parallel") | (parallel == "parallelWin")) {
        out <- parallel::parRapply(cl = cl, x = Xn, FUN = model.FUN, ...)
        
      }
      
      
      ObjsProv <- matrix(NA, nrow = npart, ncol = length(out[[1]][["Objs"]]))
      
      for (part in 1:npart) {
        if(any(is.na(out[[part]][["sim"]]))){
          
          if(iter == 1){
            ObjsProv[part,] <- out[[part]][["Objs"]]###############
            ModelOutPop[[part]] <- out[[part]][["sim"]]##################
            
          }else{
            Pop[["Position"]][part,] <- Pop.Saved[["Position"]][part,]
            ObjsProv[part,] <- Pop.Saved[["Objs"]][part,]
            ModelOutPop[[part]] <- ModelOutPop.Saved[[part]]
          }
          
  
        }else{
          ObjsProv[part,] <- out[[part]][["Objs"]]###############
          ModelOutPop[[part]] <- out[[part]][["sim"]]##################
        }
        
      }
      
      Pop[["Objs"]] <- ObjsProv
    }
    
    
    Pop.Saved <- Pop
    
    if ( (fn.name == "hydromod") | (fn.name == "hydromodInR") ) {
      ModelOutPop.Saved <- ModelOutPop
    }
    
    if(iter == 1){
      Pop[["Best.Position"]] <- Pop[["Position"]]
      Pop[["Best.Objs"]] <- Pop[["Objs"]]
      
    }else{
      best.position <- Pop[["Best.Position"]]
      best.Objs <- Pop[["Best.Objs"]]
      current.position <- Pop[["Position"]]
      current.Objs <- Pop[["Objs"]]
      index.replace <- rep(TRUE, nrow(best.Objs))
      
      for(k in 1:nrow(best.Objs)){
        opponent <- best.Objs[k,]      # the best particle in the history vs
        contender <- current.Objs[k,] # the new particle
        
        if(!any(is.na(opponent)) & !any(is.na(contender))){ #when all Objs are numerics
          if(all(sign*opponent < sign*contender)){
            index.replace[k] <- FALSE
          }else if(!all(sign*contender >= sign*opponent)){
            index.replace[k] <- sample(c(TRUE,FALSE), size = 1)
          }
        }else{ #when some NAs are in the Objs
          if(sum(is.na(opponent)) < sum(is.na(contender))){
            index.replace[k] <- FALSE
          }else{
            index.replace[k] <- TRUE
          }
        }

        
      }
      best.position[index.replace,] <- current.position[index.replace,]
      best.Objs[index.replace,] <- current.Objs[index.replace,]
      
      Pop[["Best.Position"]] <- best.position
      Pop[["Best.Objs"]] <- best.Objs
    }
    
    if(iter == 1){

      
      Rep <- vector("list", 4)
      names(Rep) <- c("Position", "Objs", "BFE", "Ranking.BFE")
      
      # First dominance evaluation (validation required)-------
      non.dominated <- rep(FALSE, nrow(Pop[["Objs"]]))
      bfe <- rep(FALSE, nrow(Pop[["Objs"]]))
      flag.na <- apply(is.na(Pop[["Objs"]]), MARGIN = 1, FUN = any)
      
      results.BFE <- BFEandDominanceOneSet(A = round(sign*Pop[["Objs"]][!flag.na,,drop = FALSE], digits.dom), 
                                           K = maxrep)
      non.dominated[!flag.na] <- results.BFE[["Selection"]]
      bfe[!flag.na] <- results.BFE[["BFE"]]
      
      Rep[["Position"]] <- Pop[["Position"]][non.dominated,,drop = FALSE]
      Rep[["Objs"]] <- Pop[["Objs"]][non.dominated,,drop = FALSE]
      Rep[["BFE"]] <- bfe[non.dominated]
      Rep[["Ranking.BFE"]] <- rank(-Rep[["BFE"]])
      
      # save model output--------------------
      if ( (fn.name == "hydromod") | (fn.name == "hydromodInR") ) {
        Rep.ModelOut <- ModelOutPop[non.dominated]
      }
      # END save model output---------------

      nobj <- ncol(Rep[["Objs"]])

      if(!is.null(model.FUN.args[["Objs.names"]])){
        obj.names <- model.FUN.args[["Objs.names"]]
      }else{
        obj.names <- paste0("Obj",1:nobj)
      }
  
      colnames(Rep[["Objs"]]) <- obj.names
      
      name.Objs <- names(out[[1]][["Objs"]])

      # minmax
      df.minmax <- data.frame(MaxMin = ifelse(sign==1, "min", "max"))

      # objs names
      df.obj.names <- data.frame(matrix(NA, ncol = nobj, nrow = 1))
      colnames(df.obj.names) <- paste0("Obj",seq(1,nobj))
      df.obj.names[1,1:nobj] <- obj.names


      # objs thresholds

      df.obj.thr <- data.frame(matrix(NA, ncol = nobj, nrow = 2))
      colnames(df.obj.thr) <- paste0("Obj",seq(1,nobj))
      df.obj.thr[1,1:nobj] <- ifelse(is.null(obj.thr), rep(NA, nobj), sapply(obj.thr,"[",1))
      df.obj.thr[2,1:nobj] <- ifelse(is.null(obj.thr), rep(NA, nobj), sapply(obj.thr,"[",2))


      if(write2disk){

        write.table(df.minmax, paste0(drty.out, "/MOPSO_MaxMin.txt"), row.names = FALSE, quote = FALSE)
        write.table(df.obj.names, paste0(drty.out, "/MOPSO_ObjsNames.txt"), row.names = FALSE, quote = FALSE)
        write.table(df.obj.thr, paste0(drty.out, "/MOPSO_ObjsThresholds.txt"), row.names = FALSE, quote = FALSE)

      }
      

      if (fn.name %in% c("hydromod", "hydromodInR")) {

        # nvar
        nvar <- length(model.FUN.args[["Obs"]])
        df.dimensions <- data.frame(nObjs = nobj, nVars = nvar)

        # Vars names
        var.names <- model.FUN.args[["var.names"]]
        var.units <- model.FUN.args[["var.units"]]
        
        df.var.names <- data.frame(matrix(NA, ncol = nvar, nrow = 1))
        colnames(df.var.names) <- c(paste0("var",seq(1,nvar)))
        if(!is.null(var.names)){
          df.var.names[1,1:nvar] <- var.names[1:nvar]
        }
        df.var.units <- data.frame(matrix(NA, ncol = nvar, nrow = 1))
        colnames(df.var.units) <- c(paste0("var",seq(1,nvar), "_unit"))
        if(!is.null(var.units)){
          df.var.units[1,1:nvar] <- var.units[1:nvar]
        }

        df.vars <- cbind.data.frame(df.var.names, df.var.units)


        # warm up

        if(!is.null(model.FUN.args[["warmup.period"]])){
          df.warmup <- data.frame(Dates = model.FUN.args[["warmup.period"]])
        }else{
          df.warmup <- data.frame(Dates = NULL)
        }
        
        


        # obs

        list.obs <- vector(mode = "list", length = nvar)

        for(i in 1:nvar){
          
          list.obs[[i]] <- model.FUN.args[["Obs"]][[i]]

        }

        df.dates.cal <- data.frame(Dates = model.FUN.args[["cal.period"]])

        
        if(write2disk){

          write.table(df.dimensions, paste0(drty.out, "/hydro_Dimensions.txt"), row.names = FALSE, quote = FALSE)
          write.table(df.vars, paste0(drty.out, "/hydro_NamesAndUnitsVars.txt"), row.names = FALSE, quote = FALSE)
          write.table(df.warmup, paste0(drty.out, "/hydro_DatesWarmUp.txt"), row.names = FALSE, quote = FALSE)
          write.table(df.dates.cal, paste0(drty.out, "/hydro_DatesCal.txt"), row.names = FALSE, quote = FALSE)


          for(i in 1:nvar){
            
            obs_var <- formatC(coredata(list.obs[[i]]), format="E", digits=digits, flag=" ")
            obs_time <- time(list.obs[[i]])
            
            write.table(data.frame(Date_Obs = obs_time, Obs = obs_var), 
                        paste0(drty.out, "/hydro_Obs_var",i,".txt"), row.names = FALSE, quote = FALSE)
            
          }


        }
      }
      
      
      
      ############################################################################  
      #                          Text Files initialisation                       #
      ############################################################################  
      if (write2disk) {

        on.exit(closeAllConnections())
        
        if (verbose) message("                                                                                ")
        if (verbose) message("================================================================================")
        if (verbose) message("[ Writing the 'NMPSO_logfile.txt' file ...                                     ]")
        if (verbose) message("================================================================================") 
        
        
        NMPSOparam.fname <- paste(file.path(drty.out), "/", "NMPSO_logfile.txt", sep="")
        NMPSOparam.TextFile  <- file(NMPSOparam.fname , "w+")
        
        writeLines("================================================================================", NMPSOparam.TextFile)  
        writeLines(c("hydroMOPSO version  :", sessionInfo()$otherPkgs$hydroMOPSO$Version), NMPSOparam.TextFile, sep="  ")
        writeLines("", NMPSOparam.TextFile) 
        writeLines(c("hydroMOPSO Built    :", sessionInfo()$otherPkgs$hydroMOPSO$Built), NMPSOparam.TextFile, sep="  ")
        writeLines("", NMPSOparam.TextFile) 
        writeLines(c("R version         :", sessionInfo()[[1]]$version.string), NMPSOparam.TextFile, sep="  ")
        writeLines("", NMPSOparam.TextFile) 
        writeLines(c("Platform          :", sessionInfo()[[1]]$platform), NMPSOparam.TextFile, sep="  ")
        writeLines("", NMPSOparam.TextFile) 
        writeLines("================================================================================", NMPSOparam.TextFile)
        Time.Ini <- Sys.time()  
        writeLines(c("Starting Time     :", date()), NMPSOparam.TextFile, sep=" ")
        writeLines("", NMPSOparam.TextFile) 
        writeLines("================================================================================", NMPSOparam.TextFile)  
        writeLines(c("Objective Function:", fn.name), NMPSOparam.TextFile, sep=" ") 
        writeLines("", NMPSOparam.TextFile) 
        writeLines(c("MinMax            :", MinMax), NMPSOparam.TextFile, sep=" ") 
        writeLines("", NMPSOparam.TextFile) 
        writeLines(c("Dimension         :", n), NMPSOparam.TextFile, sep=" ") 
        writeLines("", NMPSOparam.TextFile) 
        writeLines(c("Nmbr of Particles :", npart), NMPSOparam.TextFile, sep=" ") 
        writeLines("", NMPSOparam.TextFile) 
        writeLines(c("Max Iterations    :", maxit), NMPSOparam.TextFile, sep=" ") 
        writeLines("", NMPSOparam.TextFile) 
        writeLines(c("Boundary wall     :", boundary.wall), NMPSOparam.TextFile, sep=" ") 
        writeLines("", NMPSOparam.TextFile) 
        writeLines(c("Xini.type         :", Xini.type), NMPSOparam.TextFile, sep=" ") 
        writeLines("", NMPSOparam.TextFile) 
        writeLines(c("Vini.type         :", Vini.type), NMPSOparam.TextFile, sep=" ")
        writeLines("", NMPSOparam.TextFile)  
        writeLines(c("parallel          :", parallel), NMPSOparam.TextFile, sep=" ")  
        writeLines("", NMPSOparam.TextFile)  
        if (parallel!="none") {
          writeLines(c("par.nnodes        :", par.nnodes), NMPSOparam.TextFile, sep=" ") 
          writeLines("", NMPSOparam.TextFile)
          writeLines(c("par.pkgs          :", par.pkgs), NMPSOparam.TextFile, sep=" ") 
          writeLines("", NMPSOparam.TextFile)     
        } # IF end
        close(NMPSOparam.TextFile) 
        
        # File 'MOPSO_ParetoFront.txt' #
        PF.Textfname <- paste(file.path(drty.out), "/", "MOPSO_ParetoFront.txt", sep="")
        PF.TextFile  <- file(PF.Textfname, "w+")
        
        writeLines(c("Iter", "Phase", obj.names), PF.TextFile, sep=" ") 
        writeLines("", PF.TextFile) 
        close(PF.TextFile) 
        
        
        # File 'MOPSO_ParticlesParetoFront.txt' #
        ParticlesPF.Textfname <- paste(file.path(drty.out), "/MOPSO_ParticlesParetoFront.txt", sep="")
        ParticlesPF.TextFile  <- file(ParticlesPF.Textfname, "w+")
        
        writeLines(c("Iter", "Phase", obj.names, param.IDs), ParticlesPF.TextFile, sep=" ") 
        writeLines("", ParticlesPF.TextFile) 
        close(ParticlesPF.TextFile)
        
        if(!is.null(model.FUN.args[["Obs"]])){
          if(is.list(model.FUN.args[["Obs"]])){
            
          }
        }


        #puede ser aqui
        
        
        
        if ( (fn.name=="hydromod") | (fn.name=="hydromodInR" ) ) {
          ##############################################################################
          # 2)                           Writing Info File
          ##############################################################################  
          
          if (verbose) message("================================================================================")
          if (verbose) message("[ Writing the 'hydroMOPSO_logfile.txt' file ...                                ]")
          if (verbose) message("================================================================================") 
          
          
          # File 'hydroMOPSO_logfile.txt' #        
          hydroMOPSOparam.fname <- paste(file.path(drty.out), "/", "hydroMOPSO_logfile.txt", sep="")
          hydroMOPSOparam.TextFile  <- file(hydroMOPSOparam.fname , "w+")
          
          writeLines("================================================================================", hydroMOPSOparam.TextFile) 
          writeLines(c("Platform               :", sessionInfo()[[1]]$platform), hydroMOPSOparam.TextFile, sep="  ")
          writeLines("", hydroMOPSOparam.TextFile) 
          writeLines(c("R version              :", sessionInfo()[[1]]$version.string), hydroMOPSOparam.TextFile, sep="  ")
          writeLines("", hydroMOPSOparam.TextFile) 
          writeLines(c("hydroMOPSO version       :", sessionInfo()$otherPkgs$hydroMOPSO$Version), hydroMOPSOparam.TextFile, sep="  ")
          writeLines("", hydroMOPSOparam.TextFile) 
          writeLines(c("hydroMOPSO Built         :", sessionInfo()$otherPkgs$hydroMOPSO$Built), hydroMOPSOparam.TextFile, sep="  ")
          writeLines("", hydroMOPSOparam.TextFile) 
          writeLines("================================================================================", hydroMOPSOparam.TextFile) 
          writeLines(c("Starting Time          :", date()), hydroMOPSOparam.TextFile, sep="  ")
          writeLines("", hydroMOPSOparam.TextFile) 
          writeLines("================================================================================", hydroMOPSOparam.TextFile) 
          if (fn.name=="hydromod") {
            writeLines(c("NMPSO Input Directory    :", drty.in), hydroMOPSOparam.TextFile, sep=" ") 
            writeLines("", hydroMOPSOparam.TextFile) 
            writeLines(c("NMPSO Output Directory   :", drty.out), hydroMOPSOparam.TextFile, sep=" ") 
            writeLines("", hydroMOPSOparam.TextFile) 
            writeLines(c("Parameter Ranges       :", basename(param.ranges)), hydroMOPSOparam.TextFile, sep=" ") 
            writeLines("", hydroMOPSOparam.TextFile) 
          } # IF end  
          try(writeLines(c("hydromod function      :", model.FUN.name), hydroMOPSOparam.TextFile, sep=" ") , TRUE)
          writeLines("", hydroMOPSOparam.TextFile) 
          if ( (fn.name=="hydromod") | (fn.name=="hydromodInR") ) {
            writeLines(c("hydromod args          :"), hydroMOPSOparam.TextFile, sep=" ") 
            writeLines("", hydroMOPSOparam.TextFile) 
            for ( i in 1:length(model.FUN.args) ) {
              arg.name1  <- names(model.FUN.args)[i]
              arg.name  <- format(paste("  ", arg.name1, sep=""), width=22, justify="left" )
              arg.value <- ""
              if (arg.name1 != "param.values") 
                arg.value <- try( as.character( eval( model.FUN.args[[i]]) ), TRUE)
              writeLines(c(arg.name, ":", arg.value), hydroMOPSOparam.TextFile, sep=" ") 
              writeLines("", hydroMOPSOparam.TextFile) 
            } # FOR end
          } # IF end
          # Closing the text file
          close(hydroMOPSOparam.TextFile) 
          
        } # IF 'fn.name' END
        
      } # IF 'write2disk' end
      
      
      if (write2disk) {
        # File 'ParetoFront.txt' #
        PF.TextFile           <- file(PF.Textfname, "a") 
        # File 'ParticlesParetoFront.txt' #
        ParticlesPF.TextFile           <- file(ParticlesPF.Textfname, "a")  
      } # IF end   
      
      ###### AGREGAR AQUI

    }else{
      
      
      non.dominated <- rep(FALSE, nrow(rbind(Rep[["Position"]], Pop[["Position"]])))
      bfe <- rep(FALSE, nrow(rbind(Rep[["Position"]], Pop[["Position"]])))
      flag.na.Rep <- apply(is.na(Rep[["Objs"]]), MARGIN = 1, FUN = any)
      flag.na.Pop <- apply(is.na(Pop[["Objs"]]), MARGIN = 1, FUN = any)
      
      results.BFE <- BFEandDominanceTwoSets(A = round(sign*Rep[["Objs"]][!flag.na.Rep,,drop = FALSE], digits.dom), 
                                            S = round(sign*Pop[["Objs"]][!flag.na.Pop,,drop = FALSE], digits.dom), 
                                            K = maxrep)
      non.dominated[!c(flag.na.Rep, flag.na.Pop)] <- results.BFE[["Selection"]]
      bfe[!c(flag.na.Rep, flag.na.Pop)] <- results.BFE[["BFE"]]
      
      Rep[["Position"]] <- rbind(Rep[["Position"]], Pop[["Position"]])[non.dominated,,drop = FALSE]
      Rep[["Objs"]] <- rbind(Rep[["Objs"]], Pop[["Objs"]])[non.dominated,,drop = FALSE]
      Rep[["BFE"]] <- bfe[non.dominated]
      Rep[["Ranking.BFE"]] <- rank(-Rep[["BFE"]])
      
      # save model output--------------------
      if ( (fn.name == "hydromod") | (fn.name == "hydromodInR") ) {
        Rep.ModelOut <- c(Rep.ModelOut, ModelOutPop)[non.dominated]
      }
      # END save model output---------------
      
    }


    if(iter == 1){
      rep.history <- data.frame(rep(iter,nrow(Rep[["Objs"]])), rep(1,nrow(Rep[["Objs"]])), Rep[["Objs"]])
      colnames(rep.history) <- c("Iter", "Phase", obj.names)
      pos.history <- data.frame(rep(iter,nrow(Rep[["Objs"]])), rep(1,nrow(Rep[["Objs"]])), Rep[["Objs"]], Rep[["Position"]])
      colnames(pos.history) <- c("Iter", "Phase", obj.names, param.IDs)
    }else{
      repo <- as.data.frame(Rep[["Objs"]])
      colnames(repo) <- c(obj.names)
      posi <- as.data.frame(Rep[["Position"]])
      colnames(posi) <- c(param.IDs)
      rep.history <- rbind.data.frame(rep.history, data.frame("Iter" = rep(iter,nrow(repo)), "Phase" = rep(1,nrow(repo)), repo))
      pos.history <- rbind.data.frame(pos.history, data.frame("Iter" = rep(iter,nrow(repo)), "Phase" = rep(1,nrow(repo)), repo, posi))
    }


    if (write2disk) {
    
      for (j in 1:nrow(Rep[["Objs"]])) {
        
        Obj.set <- as.numeric(Rep[["Objs"]][j,])
        Position.set <- as.numeric(Rep[["Position"]][j,])
        
        # File 'ParetoFront.txt' #
        if(all(is.finite(Obj.set))) {
          suppressWarnings(
            writeLines(as.character(c(iter, 1, formatC(Obj.set, format="E", digits=digits, flag=" "))), PF.TextFile, sep="  ") 
          )
        } else suppressWarnings( 
            writeLines(as.character(c(iter, 1, rep("NA",nobj                                       ))), PF.TextFile, sep="  ") 
        )
        writeLines("", PF.TextFile)
        flush(PF.TextFile)
        
        # File 'ParticlesParetoFront.txt' #
        if(all(is.finite(Obj.set))) {
          suppressWarnings(
            writeLines(as.character( c(iter, 1, formatC(Obj.set, format="E", digits=digits, flag=" "), formatC(Position.set, format="E", digits=digits, flag=" "))), ParticlesPF.TextFile, sep="  ") 
          )
        } else suppressWarnings( 
            writeLines(as.character( c(iter, 1, rep("NA",nobj                                       ), formatC(Position.set, format="E", digits=digits, flag=" "))), ParticlesPF.TextFile, sep="  ") 
        )
        writeLines("", ParticlesPF.TextFile)
        flush(ParticlesPF.TextFile)
        
      }
      
    }
    

    if(plot){
      pairs(Rep[["Objs"]], col = "firebrick2", main = paste0("Pareto Front \n Iter: ", iter))

    }
    
    #===============================================================================================
    # INTERLUDE: Application of genetic operators 
    #===============================================================================================
    
    if(nrow(Rep[["Objs"]]) > 1){
      PopSS <- vector("list", 2)
      names(PopSS) <- c("Position", "Objs")
      
      if(nrow(Rep[["Position"]]) > maxcross){
        select.to.cross <- which(Rep[["Ranking.BFE"]] %in% seq(1,maxcross))
        RepCross <- Rep[["Position"]][select.to.cross,, drop = FALSE]
      }else{
        RepCross <- Rep[["Position"]]
      }
      
      newX <- GeneticOperators(Param = RepCross, lower = lower, upper = upper)
      
      Xg <- matrix(NA, ncol= ncol(newX), nrow = nrow(newX))
      colnames(Xg) <- colnames(Pop[["Position"]])
      
      for(j in 1:nrow(newX)){
        out <- SimpleBoundaryTreatment(x.new = newX[j,], x.MinMax = X.Boundaries, boundary.wall = boundary.wall)
        Xg[j,] <- out[["x.new"]]
      }
      
      PopSS[["Position"]] <- Xg
      
      nss <- nrow(PopSS[["Position"]])
      
      if ( (fn.name == "hydromod") | (fn.name == "hydromodInR") ) {
        ModelOutPopSS <- vector("list", nss)
      }
      
      Xn <- PopSS[["Position"]]
      
      cont_eval <- cont_eval + nrow(PopSS[["Position"]])
      
      #Running de model
      
      
      if ( (fn.name != "hydromod") & (fn.name != "hydromodInR") ) {
        
        # Evaluating an R Function 
        if (parallel=="none") {
          out <- apply(Xn, fn, MARGIN=1, ...)
        } else if ( (parallel=="parallel") | (parallel=="parallelWin") ) {
          out <- parallel::parRapply(cl= cl, x=Xn, FUN=fn, ...)
        } 
        
        
        ObjsProv <- matrix(NA, nrow = nss, ncol = length(out[[1]][["Objs"]]))
        
        for (part in 1:nss) {
          
          ObjsProv[part,] <- out[[part]][["Objs"]]
          
        }
        
        PopSS[["Objs"]] <- ObjsProv
        
      }else if(fn.name == "hydromod") {
        if ("verbose" %in% names(model.FUN.args)) {
          verbose.FUN <- model.FUN.args[["verbose"]]
        }
        else verbose.FUN <- verbose
        if (parallel == "none") {
          out <- lapply(1:nss, hydromodEval, Particles = Xn, iter = iter, npart = nss, maxit = maxit, REPORT = REPORT, verbose = verbose.FUN, 
                        digits = digits, model.FUN = model.FUN, model.FUN.args = model.FUN.args, parallel = parallel, ncores = par.nnodes, part.dirs = mc.dirs)
        }
        else if ((parallel == "parallel") | (parallel == "parallelWin")) {
          out <- parallel::clusterApply(cl = cl, x = 1:nss, fun = hydromodEval, Particles = Xn, iter = iter, npart = nss, maxit = maxit, REPORT = REPORT, 
                                        verbose = verbose.FUN, digits = digits, model.FUN = model.FUN, model.FUN.args = model.FUN.args, parallel = parallel, 
                                        ncores = par.nnodes, part.dirs = part.dirs)
        }
        
        ObjsProv <- matrix(NA, nrow = nss, ncol = length(out[[1]][["Objs"]]))
        
        for (part in 1:nss) {
          
          ObjsProv[part,] <- out[[part]][["Objs"]]
          ModelOutPopSS[[part]] <- out[[part]][["sim"]]
        }
        
        PopSS[["Objs"]] <- ObjsProv
        
      }else if (fn.name == "hydromodInR") {
        if (parallel == "none") {
          out <- apply(Xn, model.FUN, MARGIN = 1, ...)
        }
        else if ((parallel == "parallel") | (parallel == "parallelWin")) {
          out <- parallel::parRapply(cl = cl, x = Xn, FUN = model.FUN, ...)
        }
        
        ObjsProv <- matrix(NA, nrow = nss, ncol = length(out[[1]][["Objs"]]))
        
        for (part in 1:nss) {
          ObjsProv[part,] <- out[[part]][["Objs"]]
          ModelOutPopSS[[part]] <- out[[part]][["sim"]]
        }
        
        PopSS[["Objs"]] <- ObjsProv

      }
    
      #actualizacion de las posiciones
      
      if ( (fn.name == "hydromod") | (fn.name == "hydromodInR") ) {
        #PopSS.Valid <- !sapply(lapply(ModelOutPopSS, FUN = is.na), FUN = any)
        PopSS.Valid <- !sapply(lapply(ModelOutPopSS, FUN = lapplyNA), FUN = any)
      }else{
        PopSS.Valid <- !sapply(lapply(PopSS, FUN = is.na), FUN = any)
      }
      
      PopSS[["Position"]] <- PopSS[["Position"]][PopSS.Valid,,drop = FALSE]
      PopSS[["Objs"]] <- PopSS[["Objs"]][PopSS.Valid,,drop = FALSE]
      
      if ( (fn.name == "hydromod") | (fn.name == "hydromodInR")) {
        ModelOutPopSS <- ModelOutPopSS[PopSS.Valid]
      }
      
      if(any(PopSS.Valid)){
        
        results.BFE.SS <- BFEandDominanceOneSet(A = round(sign*PopSS[["Objs"]], digits.dom), 
                                                K = maxrep)
        non.dominated.SS <- results.BFE.SS[["Selection"]]
        
        PopSS.Position.Sel <- PopSS[["Position"]][non.dominated.SS,,drop = FALSE]
        PopSS.Objs.Sel <- PopSS[["Objs"]][non.dominated.SS,,drop = FALSE]
        
        results.BFE <- BFEandDominanceTwoSets(A = round(sign*Rep[["Objs"]], digits.dom), 
                                              S = round(sign*PopSS.Objs.Sel, digits.dom), 
                                              K = maxrep)
        non.dominated <- results.BFE[["Selection"]]
        bfe <- results.BFE[["BFE"]]
        
        Rep[["Position"]] <- rbind(Rep[["Position"]], PopSS.Position.Sel)[non.dominated,,drop = FALSE]
        Rep[["Objs"]] <- rbind(Rep[["Objs"]], PopSS.Objs.Sel)[non.dominated,,drop = FALSE]
        Rep[["BFE"]] <- bfe[non.dominated]
        Rep[["Ranking.BFE"]] <- rank(-Rep[["BFE"]])
        
        # save model output--------------------
        if ( (fn.name == "hydromod") | (fn.name == "hydromodInR") ) {
          Rep.ModelOut <- c(Rep.ModelOut, ModelOutPopSS)[non.dominated]
        }
        # END save model output---------------
        
      }
    }
    
    repo <- as.data.frame(Rep[["Objs"]])
    colnames(repo) <- c(obj.names)
    posi <- as.data.frame(Rep[["Position"]])
    colnames(posi) <- c(param.IDs)
    rep.history <- rbind.data.frame(rep.history, data.frame("Iter" = rep(iter,nrow(repo)), "Phase" = rep(2,nrow(repo)), repo))
    pos.history <- rbind.data.frame(pos.history, data.frame("Iter" = rep(iter,nrow(repo)), "Phase" = rep(2,nrow(repo)), repo, posi))
  
    if (write2disk) {

      for (j in 1:nrow(Rep[["Objs"]])) {
        
        Obj.set <- as.numeric(Rep[["Objs"]][j,])
        Position.set <- as.numeric(Rep[["Position"]][j,])
        
        # File 'ParetoFront.txt' #
        if(all(is.finite(Obj.set))) {
          suppressWarnings(
            writeLines(as.character(c(iter, 2, formatC(Obj.set, format="E", digits=digits, flag=" "))), PF.TextFile, sep="  ") 
          )
        } else suppressWarnings(
            writeLines(as.character(c(iter, 2, rep("NA",nobj                                         ))), PF.TextFile, sep="  ") 
        )
        writeLines("", PF.TextFile)
        flush(PF.TextFile)
        
        # File 'ParticlesParetoFront.txt' #
        if(all(is.finite(Obj.set))) {
          suppressWarnings(
            writeLines(as.character(c(iter, 2, formatC(Obj.set, format="E", digits=digits, flag=" "), formatC(Position.set, format="E", digits=digits, flag=" "))), ParticlesPF.TextFile, sep="  ") 
          )
        } else suppressWarnings(
            writeLines(as.character(c(iter, 2, rep("NA",nobj                                       ), formatC(Position.set, format="E", digits=digits, flag=" "))), ParticlesPF.TextFile, sep="  ") 
        )
        writeLines("", ParticlesPF.TextFile)
        flush(ParticlesPF.TextFile)
        
      }
      
    } 

    if(plot){
      pairs(Rep[["Objs"]], col = "firebrick2", main = paste0("Pareto Front \n Iter: ", iter))


 #     if(length(Rep[["BFE"]])>2){
 #       hist(Rep[["BFE"]], 12, xlim = c(0,1), main = round(sd(Rep[["BFE"]])/mean(Rep[["BFE"]]),2))
 #     }

    }

    #===============================================================================================
    # END Interlude
    #=============================

    if(cal.hv){
      hv <- HV(data  = sign*Rep[["Objs"]], nadir.point.PF = sign*nadir.point, n.samples = samples)
      df.hv[iter,2] <- cont_eval
      df.hv[iter,3] <- hv
      msg.hv <- paste0("  HV:", format(hv, width = nchar(maxrep), justify = "right"))
    }else{
      msg.hv <- ""
    }
    
    if ((verbose) & (iter/REPORT == floor(iter/REPORT))){


      utopia <- matrix(rep(apply(sign*Rep[["Objs"]], MARGIN = 2, FUN = min), nrow(sign*Rep[["Objs"]])),
                     ncol = ncol(sign*Rep[["Objs"]]), nrow = nrow(sign*Rep[["Objs"]]), byrow = TRUE)
    
      distance <- sqrt(apply((utopia - sign*Rep[["Objs"]])^2, MARGIN = 1, FUN = sum))

      bestcomp <- round(Rep[["Objs"]][which.min(distance),], 4)

      bestobjs <- round(sign*apply(sign*Rep[["Objs"]], MARGIN = 2, FUN = min),4)

      
      char_a <- rep("best(", length = length(obj.names))
      char_b <- rep("):", length = length(obj.names))
      char_c <- rep("   ", length = length(obj.names))
      char_d <- rep(":", length = length(obj.names))
      

      bestobjs2print <- paste(apply(matrix(c(char_a, obj.names, char_b, formatC(bestobjs, format="E", digits=4, flag=" "), char_c), ncol = 5), FUN = paste0, MARGIN = 1), collapse = "")
      bestcomp2print <- paste(apply(matrix(c(obj.names, char_d, formatC(bestcomp, format="E", digits=4, flag=" "), char_c), ncol = 4), FUN = paste0, MARGIN = 1), collapse = "")


      suppressWarnings(message(" iter: ", format(iter, width = nchar(maxit), justify = "right"),
                               " NRep: ", format(nrow(Rep[["Position"]]), width = nchar(maxrep), justify = "right"),
                               " Evaluations: ", format(cont_eval, width = nchar(maxeval), justify = "right"),
                               "   ", bestobjs2print,
                               " ||   ",
                               " Best Compromise >>  ", bestcomp2print,
                               msg.hv))
    }

    
    iter <- iter + 1
    
    if(iter > maxit || cont_eval > maxeval){
      break
    }
    
    # END of each iteration=====
    # END===========================
    # END================================
    
    
    TopRepIndex <- which(Rep[["Ranking.BFE"]] <= as.numeric(quantile(Rep[["Ranking.BFE"]], probs = 0.1, type = 3)))
    
    if(length(TopRepIndex)>1){
      LeaderIndex <- sample(x = TopRepIndex, size = nrow(Pop[["Position"]]), replace = TRUE)
    }else{
      LeaderIndex <- rep(TopRepIndex, nrow(Pop[["Position"]]))
    }
  
    
    Pop.Position <- Pop[["Position"]]
    Pop.Velocity <- Pop[["Velocity"]]
    Leader <- Rep[["Position"]][LeaderIndex,]
    Pop.BestPosition <- Pop[["Best.Position"]]
    
    newV <- matrix(NA, ncol= ncol(Pop.Velocity), nrow = nrow(Pop.Velocity))
    
    for(j in 1:npart){
      
      w <- rep(runif(1, min = 0.1, max = 0.5), times = n)
      
      # r1 <- rep(runif(1, min=0, max=1), times = n)
      # r2 <- rep(runif(1, min=0, max=1), times = n)
      # r3 <- rep(runif(1, min=0, max=1), times = n)
      
      r1 <- runif(n, min=0, max=1)
      r2 <- runif(n, min=0, max=1)
      r3 <- runif(n, min=0, max=1)
      
      c1 <- rep(runif(1, min = 1.5, max = 2.5), times = n)
      c2 <- rep(runif(1, min = 1.5, max = 2.5), times = n)
      c3 <- rep(runif(1, min = 1.5, max = 2.5), times = n)
      
      newV[j,] <- w*Pop.Velocity[j,] + c1*r1*(Pop.BestPosition[j,] - Pop.Position[j,]) + c2*r2*(Leader[j,] - Pop.Position[j,])# + c3*r3*(Leader[j,] - Pop.BestPosition[j,])
    }
    #
    X <- matrix(NA, ncol= ncol(Pop.Position), nrow = nrow(Pop.Position))
    colnames(X) <- colnames(Pop.Position)
    V <- matrix(NA, ncol= ncol(Pop.Velocity), nrow = nrow(Pop.Velocity))
    colnames(V) <- colnames(Pop.Velocity)
    
    for(j in 1:nrow(newV)){
      out <- PositionIpdate(x = Pop.Position[j,], v = newV[j,], x.MinMax = X.Boundaries, boundary.wall = boundary.wall)
      
      X[j,] <- out[["x.new"]]
      V[j,] <- out[["v.new"]]
    }
    
  }
  
  
  if (write2disk) {
    # File 'ParetoFront.txt' #
    close(PF.TextFile)
    # File 'ParticlesParetoFront.txt' #
    close(ParticlesPF.TextFile)
  } # IF end
  
  
  ###################   START WRITING OUTPUT FILES     ###################
  if (write2disk) {
    
    if (verbose) message("                           ")
    if (verbose) message("[ Writing output files... ]")
    if (verbose) message("                           ")
    
    niter.real <- iter - 1 
    
    NMPSOparam.TextFile <- file(NMPSOparam.fname, "a")    
    
    writeLines("================================================================================", NMPSOparam.TextFile) 
    writeLines(c("Total fn calls    :", cont_eval-1), NMPSOparam.TextFile, sep="  ")
    writeLines("", NMPSOparam.TextFile) 
    writeLines(c("Nmbr of Iterations:", iter-1), NMPSOparam.TextFile, sep="  ")
    writeLines("", NMPSOparam.TextFile) 
    writeLines("================================================================================", NMPSOparam.TextFile) 
    writeLines(c("Ending Time       :", date()), NMPSOparam.TextFile, sep="  ")
    writeLines("", NMPSOparam.TextFile)
    Time.Fin <- Sys.time() 
    writeLines("================================================================================", NMPSOparam.TextFile) 
    writeLines(c("Elapsed Time      :", format(round(Time.Fin - Time.Ini, 2))), NMPSOparam.TextFile, sep="  ")
    writeLines("", NMPSOparam.TextFile) 
    writeLines("================================================================================", NMPSOparam.TextFile) 
    close(NMPSOparam.TextFile)
    

  
    if ( (fn.name=="hydromod") | (fn.name=="hydromodInR") ) {
      
      hydroMOPSOparam.TextFile <- file(hydroMOPSOparam.fname, "a")    
      
      writeLines("================================================================================", NMPSOparam.TextFile) 
      writeLines(c("Total model calls      :", cont_eval-1), NMPSOparam.TextFile, sep="  ")
      writeLines("", NMPSOparam.TextFile) 
      writeLines("================================================================================", hydroMOPSOparam.TextFile) 
      writeLines(c("Ending Time            :", date()), hydroMOPSOparam.TextFile, sep=" ")
      writeLines("", hydroMOPSOparam.TextFile) 
      writeLines("================================================================================", hydroMOPSOparam.TextFile) 
      writeLines(c("Elapsed Time           :", format(round(Time.Fin - Time.Ini, 2))), hydroMOPSOparam.TextFile, sep=" ")
      writeLines("", hydroMOPSOparam.TextFile) 
      writeLines("================================================================================", hydroMOPSOparam.TextFile) 
      close(hydroMOPSOparam.TextFile)
      
    } # IF 'fn.name' END           
    
  } # IF end
  #####################     END WRITING OUTPUT FILES     #####################
  
  ########################################################################
  ##                                parallel                             #
  ########################################################################
  if (parallel!="none") {
    if(parallel %in% c("parallel", "parallelWin")){
      if (verbose) message("[ Stopping parallelisation ... ]")
      if (verbose) message("[ (Probably, you will see some 'closing unused connection' warning messages, don't worry about them)... ]")  
      parallel::stopCluster(cl) 
      closeAllConnections()
    }     
    if(fn.name == "hydromod"){
      if (verbose) message("                                         ")
      if (verbose) message("[ Removing the 'parallel' directory ... ]")
      unlink(dirname(mc.dirs[1]), recursive=TRUE)
    } # IF end
    
  } # IF end
  # 
  # #Crear el output : Repositorio
  #

  MOPSOResults <- list("ParetoFront" = rep.history, 
                       "ParticlesParetoFront" = pos.history,
                       "ObjsNames" = df.obj.names,
                       "MaxMin" = df.minmax,
                       "ObjsThreshold" = df.obj.thr
                       )


  if(fn.name %in% c("hydromod", "hydromodInR")){


    hydroDetails <- list("Obs" = list.obs, # hyd
                         "Dimensions" = df.dimensions, # hyd
                         "NamesAndUnitsVars" = df.vars, # hyd
                         "WarmUp" = df.warmup, # hyd
                         "DatesCal" = df.dates.cal # hyd
                         )

    hydroResults <- PostResults(MOPSO.Results = MOPSOResults,
                               hydro.Details = hydroDetails,
                               fn = fn.name,
                               control = control[c("drty.in", "drty.out", "write2disk", "verbose", "REPORT", "digits", "digits.dom", "parallel", "par.nnodes", "par.pkgs")],
                               model.FUN = model.FUN,
                               analysis.period = "calibration",
                               model.FUN.args = model.FUN.args)[["hydroResults"]]

    if(cal.hv){
      out <- list(Rep, MOPSOResults, hydroDetails, hydroResults, df.hv)
      names(out) <- c("Rep", "MOPSOResults", "hydroDetails", "hydroResults", "HV")
    }else{
      out <- list(Rep, MOPSOResults, hydroDetails, hydroResults)
      names(out) <- c("Rep", "MOPSOResults", "hydroDetails", "hydroResults")
    }

  }else{

    if(cal.hv){
      out <- list(Rep, MOPSOResults, df.hv)
      names(out) <- c("Rep", "MOPSOResults", "HV")
    }else{
      out <- list(Rep, MOPSOResults)
      names(out) <- c("Rep", "MOPSOResults")
    }

  }

  return(out)

} #END

# References

# Lin, Q., Liu, S., Zhu, Q., Tang, C., Song, R., Chen, J., Coello, C. A. C., Wong, K.-C., & Zhang, J. (2018). Particle Swarm Optimization With a Balanceable Fitness Estimation for Many-Objective Optimization Problems. IEEE Transactions on Evolutionary Computation, 22(1), 32-46. doi:10.1109/TEVC.2016.2631279

# Marinao-Rivas, R., & Zambrano-Bigiarini, M. (2021). Towards best default configuration settings for NMPSO in Multiobjective Optimization. 2021 IEEE Latin American Conference on Computational Intelligence. (Accepted).

# Zambrano-Bigiarini, M.; R. Rojas (2013), A model-independent Particle Swarm Optimization software for model calibration, Environmental Modelling & Software, 43, 5-25, doi:10.1016/j.envsoft.2013.01.004

# Coello, C. A. C., & Lechuga, M. S. (2002). MOPSO: A proposal for multiple objective particle swarm optimization. Proceedings of the 2002 Congress on Evolutionary Computation, CEC 2002, 2, 1051-1056. doi:10.1109/CEC.2002.1004388

# Kennedy, J., & Eberhart, R. (1995). Particle swarm optimization. Proceedings of ICNN'95 - International Conference on Neural Networks, 4, 1942-1948. doi:10.1109/ICNN.1995.488968

# Deb, K. (1999). Multi-objective genetic algorithms: problem difficulties and construction of test problems. Evolutionary computation, 7, 205-230. doi:10.1162/EVCO.1999.7.3.205

# Kursawe, F. (1991). A variant of evolution strategies for vector optimization. Lecture Notes in Computer Science (including subseries Lecture Notes in Artificial Intelligence and Lecture Notes in Bioinformatics), 496 LNCS, 193-197. doi:10.1007/BFB0029752

# Deb, K., Thiele, L., Laumanns, M., & Zitzler, E. (2005). Scalable Test Problems for Evolutionary Multiobjective Optimization (bll 105-145; A. Abraham, L. Jain, & R. Goldberg, Reds). doi:10.1007/1-84628-137-7_6
