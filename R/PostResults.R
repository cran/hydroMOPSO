###############################################################################################
##                                        PostResults                                        ##
###############################################################################################
# Author : Rodrigo Marinao Rivas                                                             ##
###############################################################################################
# Created: during 2021                                                                       ##
###############################################################################################
#
# Description: Function for post-processing the results of a model optimisation (i.e., when
#              fn %in% c("hydromod", "hydromodInR")), formulated to go through all the 
#              non-dominated solutions found during all the iterations made in the optimisation
#              and return results in calibration or verification.

PostResults <- function(MOPSO.Results = NULL,     # >> 'list' or 'NULL'. List with results of the optimisation itself, delivered by hydroMOPSO. When
                                                  # this atgument is NULL, the function will look for the results on disk (in the 'drty.out' 
                                                  # directory)
                        hydro.Details = NULL,     # >> 'list' or 'NULL'. List with details of the optimisation that have to do with the modeling 
                                                  # itself, registered hydroMOPSO in the optimisation. When the argument is NULL, the function will
                                                  # look for the details on disk (in the 'drty.out' directory)
                        analysis.period = NULL,   # >> 'character'. Will the results be post-processed in calibration period
                                                  # (analysis.period = "calibration") or verification period (analysis.period = "verification")?
                        fn = NULL,                # >> 'character/function'. Either a character or a function indicating the function (forgive the 
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
  

  if (missing(fn)){
    
    stop("Missing argument: 'fn' must be provided")
    
  }else if (is.character(fn) | is.function(fn)) {
    
    if (is.character(fn)) {
      
      if (fn == "hydromod") {
        fn.name <- fn
      }
      else if (fn == "hydromodInR") {
        fn.name <- fn
      }
      else {
        stop("Invalid argument: 'fn' must be in c('hydromod', 'hydromodInR')")
      }
    }else if (is.function(fn)) {
      fn.name <- as.character(substitute(fn))
    }
  }else{
    stop("Missing argument: 'class(fn)' must be in c('function', 'character')")
  }

  if(!(analysis.period %in% c("calibration", "verification"))){
    stop("Invalid argument: 'analysis.period' must be in c('calibration', 'verification')")
  }
  
  con <- list(drty.in = "MOPSO.in",
              drty.out = "MOPSO.out", 
              write2disk=FALSE,
              verbose = TRUE,
              REPORT = 1,
              digits = 8,
              digits.dom = Inf,
              parallel = c("none", "parallel", "parallelWin"),
              par.nnodes = NA,
              par.pkgs = c()
  )
  parallel <- match.arg(control[["parallel"]], con[["parallel"]])
  nmsC <- names(con)
  con[(namc <- names(control))] <- control
  
  # if (length(noNms <- namc[!namc %in% nmsC])){
  #   warning("[Unknown names in control: ", paste(noNms, collapse = ", "), " (not used) !]")
  # }
  
  
  drty.in <- con[["drty.in"]]
  drty.out <- con[["drty.out"]]
  write2disk <- as.logical(con[["write2disk"]])
  verbose <- as.logical(con[["verbose"]])
  REPORT <- con[["REPORT"]]
  digits <- con[["digits"]]
  digits.dom <- con[["digits.dom"]]
  par.nnodes <- con[["par.nnodes"]]
  par.pkgs <- con[["par.pkgs"]]


  if(write2disk){
    if(!dir.exists(paste0(drty.out, "/", analysis.period))){
      dir.create(paste0(drty.out, "/", analysis.period))
    }
  }
  
  cool.check.a1 <- FALSE
  cool.check.a2 <- FALSE
  cool.check.b1 <- FALSE
  cool.check.b2 <- FALSE


  if(!is.null(MOPSO.Results)){
    if(all(c("ParetoFront", "ParticlesParetoFront", "MaxMin", "ObjsNames", "ObjsThreshold") %in% names(MOPSO.Results))){
      cool.check.a1 <- TRUE
    }
  }

  if(!is.null(hydro.Details)){
    if(all(c("Dimensions", "NamesAndUnitsVars", "Obs", "WarmUp", "DatesCal") %in% names(hydro.Details))){
      cool.check.a2 <- TRUE
    }
  }

  if(!cool.check.a1){

    if(dir.exists(drty.out)){

      message(paste0("[ MOPSO.Results not entered in function, checking directory '",drty.out,"' ... ]"))

      required.MOPSO.input <- c(paste0(drty.out, "/MOPSO_ParetoFront.txt"),
                                paste0(drty.out, "/MOPSO_ParticlesParetoFront.txt"),
                                paste0(drty.out, "/MOPSO_MaxMin.txt"),
                                paste0(drty.out, "/MOPSO_ObjsNames.txt"),
                                paste0(drty.out, "/MOPSO_ObjsThresholds.txt"))

      input.check.MOPSO <- all(sapply(required.MOPSO.input , FUN = file.exists))

      if(input.check.MOPSO){

        cool.check.b1 <- TRUE

      }

    }

  }

  if(!cool.check.a2){

    if(dir.exists(drty.out)){

      message(paste0("[ hydro.Details not entered in function, checking directory '",drty.out,"' ... ]"))

      required.hydro.input <- c(paste0(drty.out, "/hydro_Dimensions.txt"),
                                paste0(drty.out, "/hydro_NamesAndUnitsVars.txt"),
                                paste0(drty.out, "/hydro_DatesWarmUp.txt"),
                                paste0(drty.out, "/hydro_DatesCal.txt"))

      input.check.hydro <- all(sapply(required.hydro.input , FUN = file.exists))

      if(input.check.hydro){

        cool.check.b2 <- TRUE

      }

    }

  }


  if(!cool.check.a1 & !cool.check.b1){

    message(paste0("[ MOPSO.Results not found in directory '",drty.out,"' either ... ]"))
    message(paste0("[ MOPSO.Results are required to proceed ... ]"))
    stop("[ Stopping ... ]")
  }
  
  if(!cool.check.a2 & !cool.check.b2){

    message(paste0("[ hydro.Details not found in directory '",drty.out,"' either ... ]"))
    message(paste0("[ hydro.Details are required to proceed ... ]"))
    stop("[ Stopping ... ]")
  }
  

  if(all(cool.check.a1, cool.check.a2)){

    rep.history <- MOPSO.Results[["ParetoFront"]]
    pos.history <- MOPSO.Results[["ParticlesParetoFront"]]
    df.minmax <- MOPSO.Results[["MaxMin"]]
    df.obj.names <- MOPSO.Results[["ObjsNames"]]
    df.obj.thr <- MOPSO.Results[["ObjsThreshold"]]

    df.dimensions <- hydro.Details[["Dimensions"]]
    df.vars <- hydro.Details[["NamesAndUnitsVars"]]
    df.warmup <- hydro.Details[["WarmUp"]]
    df.dates.cal <- hydro.Details[["DatesCal"]]

    if(analysis.period == "calibration"){

      list.obs <- hydro.Details[["Obs"]]

    }else if(analysis.period == "verification"){

      list.obs <- vector(mode = "list", length = df.dimensions[,2])
      for(i in 1:df.dimensions[,2]){
        list.obs[[i]] <- model.FUN.args[["Obs"]][[i]]
      }

      if(!is.null(model.FUN.args[["warmup.period"]])){
        df.warmup <- data.frame(Dates = model.FUN.args[["warmup.period"]])
      }else{
        df.warmup <- data.frame(Dates = NULL)
      }

    }


  }else if(all(cool.check.b1, cool.check.b2)){


    rep.history <- read.table(paste0(drty.out, "/MOPSO_ParetoFront.txt"), header = TRUE)
    pos.history <- read.table(paste0(drty.out, "/MOPSO_ParticlesParetoFront.txt"), header = TRUE)
    df.minmax <- read.table(paste0(drty.out, "/MOPSO_MaxMin.txt"), header = TRUE)
    df.obj.names <- read.table(paste0(drty.out, "/MOPSO_ObjsNames.txt"), header = TRUE)
    df.obj.thr <- read.table(paste0(drty.out, "/MOPSO_ObjsThresholds.txt"), header = TRUE)


    df.dimensions <- read.table(paste0(drty.out, "/hydro_Dimensions.txt"), header = TRUE)
    df.vars <- read.table(paste0(drty.out, "/hydro_NamesAndUnitsVars.txt"), header = TRUE)


    if(analysis.period == "calibration"){

      list.obs <- vector(mode = "list", length = df.dimensions[,2])
      for(i in 1:df.dimensions[,2]){
        observation_x <- read.table(paste0(drty.out, "/hydro_Obs_var",i,".txt"), header = TRUE)
        list.obs[[i]] <- zoo(observation_x[,-1], as.Date(observation_x[,1]))#
      }

      df.warmup <- read.table(paste0(drty.out, "/hydro_DatesWarmUp.txt"), header = TRUE)
      
    }else if(analysis.period == "verification"){

      list.obs <- vector(mode = "list", length = df.dimensions[,2])
      for(i in 1:df.dimensions[,2]){
        list.obs[[i]] <- model.FUN.args[["Obs"]][[i]]
      }

      if(!is.null(model.FUN.args[["warmup.period"]])){
        df.warmup <- data.frame(Dates = model.FUN.args[["warmup.period"]])
      }else{
        df.warmup <- data.frame(Dates = NULL)
      }

    }

    df.dates.cal <- read.table(paste0(drty.out, "/hydro_DatesCal.txt"), header = TRUE)

  }



  if(analysis.period == "verification"){
    if(write2disk){

    for(i in 1:df.dimensions[,2]){
      
        obs_var <- formatC(coredata(list.obs[[i]]), format="E", digits=digits, flag=" ")
        obs_time <- time(list.obs[[i]])
        
        write.table(data.frame(Date_Obs = obs_time, Obs = obs_var), 
                    paste0(drty.out, "/", analysis.period, "/hydro_Obs_var",i,".txt"), row.names = FALSE, quote = FALSE)
        
      }

      write.table(df.warmup, paste0(drty.out, "/", analysis.period, "/hydro_DatesWarmUp.txt"), row.names = FALSE, quote = FALSE)


    }
  }
  
  
  obj.dim <- df.dimensions[,1]
  var.dim <- df.dimensions[,2]  
  MaxMin <- df.minmax[1,1]

  # best.gof.post.files <- paste0(drty.out, "/hydro_Best_Obj",seq(1,gof.dim),"_Particle.txt")
  # best.gof.modelout.post.files <- paste0(drty.out, "/hydro_Best_Obj",seq(1,gof.dim),"_ModelOut.txt")
  # var.post.files <- paste0(drty.out, "/hydro_var",seq(1,var.dim),"_from_FilledPOF.txt")

  if(is.null(fn.name)){
    stop("The 'fn.name' argument must be specified")
  }else{
    if(!any(c('hydromod','hydromodInR') %in% fn.name)){
      stop("The 'fn.name' argument must be in c('hydromod','hydromodInR')")
    }
  }
  
  if(is.null(model.FUN)){
    stop("The 'model.FUN' argument must be specified.")
  }
  
  if(is.null(model.FUN.args)){
    stop("The 'model.FUN.args' argument also must be specified.")
  }
  
  #reading Pareto Front in all iterations
  PF <- rep.history
  number.objs <- ncol(PF) - 2
  
  #reading the position of the particles of the Pareto Front in all iterations
  particles.PF <- pos.history
  
  #maximisation or minimisation (question mark)
  sign <- ifelse(MaxMin == "min", 1, -1)
  
  #assigning id to ach particle in PF
  id.particles.PF <- cbind("id" = seq(1,nrow(particles.PF)), PF)

  #df.particles.full
  df.particles.full <- data.frame("Sim" = seq(1,nrow(particles.PF)), pos.history[,-c(1,2)])

  
  #filtering particles from defined digits (number of decimals)
  flag.dup <- apply(apply(round(PF[,-c(1,2)], digits = digits.dom), MARGIN = 2, FUN = duplicated), MARGIN = 1, FUN = all)
  particles.PF.filt <- id.particles.PF[!flag.dup,]
  
  #ordering the index of the filtered particles
  index.iter <- sort(unique(particles.PF.filt[,2]), decreasing = TRUE)  # mod rmr
  
  #Defining the starting full-Pareto-Front
  As <- particles.PF.filt[particles.PF.filt[,2] == index.iter[1],]   # mod rmr
  A <- sign*As[,-c(1,2,3)]


  #Updating de full-Pareto-Front
  for(k in 2:length(index.iter)){
    flag.selection <- particles.PF.filt[,2] == index.iter[k]   # mod rmr
    if(sum(flag.selection) == 1){
      Ss <- particles.PF.filt[flag.selection,,drop = FALSE]
    }else{
      Ss <- apply(particles.PF.filt[flag.selection,], MARGIN = 2, FUN = rev)
    }
    
    S <- sign*Ss[,-c(1,2,3)]
    Combine <- rbind(A,S) # m
    Choose <- c(1:nrow(A))
    for(i in 1:nrow(S)){
      mark <- vector(mode = "logical", length = length(Choose) + 1)
      for(j in 1:length(Choose)){
        flag <- any(S[i,]<Combine[Choose[j],]) - any(S[i,]>=Combine[Choose[j],])
        if(flag == 1){
          mark[j] <- TRUE
        }else if(flag == -1){
          mark[length(mark)] <- TRUE
          break
        }
      }
      Choose <- Choose[!mark[-length(mark)]]
      if(!mark[length(mark)]){
        Choose <- c(Choose, nrow(A)+i)
      }
    }
    As <- rbind(As, Ss)[Choose,]
    A <- rbind(A,S)[Choose,]
  }
  
  As <- apply(As, MARGIN = 2, FUN = rev)
  
  id.PF.select <- As[,1]
  
  particles.PF.select <-particles.PF[id.PF.select,]

  matrix.objs.num.cal <- particles.PF.select[,3:(2+obj.dim)]
  
  particles.PF.clean <- as.matrix(particles.PF.select[,-c(1:(number.objs+2))])
 
  #############
  
  model.FUN.argsDefaults <- formals(model.FUN)
  model.FUN.args <- modifyList(model.FUN.argsDefaults, model.FUN.args)
  
  model.FUN <- match.fun(model.FUN)

  formals(model.FUN) <- model.FUN.args
  
  npart.in.pof <- nrow(particles.PF.clean)

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
        logfile.fname <- paste0(file.path(drty.out), "/", analysis.period, "/hydro_parallel_logfile.txt")
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
      
      if (par.nnodes > npart.in.pof) {
        warning("[ 'par.nnodes' > npart.in.pof (", par.nnodes, ">", npart.in.pof, ") =>  par.nnodes=", npart.in.pof, " ] !")
        par.nnodes <- npart.in.pof
      }
      
      if (verbose) {
        message("[ Number of cores/nodes used    : ",  par.nnodes, " ]")
      }
      if(parallel == "parallel"){
        ifelse(write2disk, 
               cl <- parallel::makeForkCluster(nnodes = par.nnodes, outfile=logfile.fname),
               cl <- parallel::makeForkCluster(nnodes = par.nnodes) )
        
      }else if (parallel == "parallelWin"){
        ifelse(write2disk,
               cl <- parallel::makePSOCKcluster(names = par.nnodes, outfile=logfile.fname),
               cl <- parallel::makePSOCKcluster(names = par.nnodes) )
        
        pckgFn <- function(packages){
          for (i in packages) library(i, character.only = TRUE)
        }
        
        parallel::clusterCall(cl, pckgFn, par.pkgs)
        parallel::clusterExport(cl, ls.str(mode = "function", envir = .GlobalEnv))
        
        if ( (fn.name=="hydromod") | (fn.name == "hydromodInR") ) {
          fn.default.vars <- as.character(formals(model.FUN))
          parallel::clusterExport(cl, fn.default.vars[fn.default.vars %in% ls(.GlobalEnv)])
        } # IF end
             
        
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
          
          tmp       <- ceiling(npart.in.pof/par.nnodes)        
          part.dirs <- rep(mc.dirs, tmp)[1:npart.in.pof]  
        } # ELSE end                 
      } # IF end
    }
  }
  
  

  tittxt <- format(analysis.period, width=12, justify="left")

  if (verbose) message("                                                                                ")
  if (verbose) message("================================================================================")
  if (verbose) message(paste0(
                       "[               Post-process results in ", tittxt, " ...                       ]"))
  if (verbose) message("================================================================================")
  if (verbose) message("                                                                                ")


  if (fn.name == "hydromod") {
    
    # Evaluating an hydromod Function ------------------------------------------
    # --------------------------------------------------------------------------
    
    if ("verbose" %in% names(model.FUN.args)) {
      verbose.FUN <- model.FUN.args[["verbose"]]
    }
    else verbose.FUN <- verbose
    
    
    
    
    if (parallel == "none") {
      
      
      out <- lapply(1:npart.in.pof, 
                    hydromodEval, 
                    Particles = particles.PF.clean,
                    iter = 1, 
                    npart = npart.in.pof, 
                    maxit = 1, 
                    REPORT = REPORT, 
                    verbose = TRUE, 
                    digits = digits, 
                    model.FUN = model.FUN,
                    model.FUN.args = model.FUN.args,
                    parallel = "none", 
                    ncores = 1, 
                    part.dirs = mc.dirs)
      
    }
    else if ((parallel == "parallel") | (parallel == "parallelWin")) {
      out <- parallel::clusterApply(cl = cl, 
                                    x = 1:npart.in.pof,
                                    fun = hydromodEval, 
                                    Particles = particles.PF.clean, 
                                    iter = 1,
                                    npart = npart.in.pof, 
                                    maxit = 1,
                                    REPORT = REPORT, 
                                    verbose = TRUE, 
                                    digits = digits,
                                    model.FUN = model.FUN, 
                                    model.FUN.args = model.FUN.args,
                                    parallel = parallel, 
                                    ncores = par.nnodes,
                                    part.dirs = part.dirs)
    }
    
    
  }else if (fn.name == "hydromodInR") {
    
    # Evaluating an hydromodInR Function ---------------------------------------
    # --------------------------------------------------------------------------
    
    if (parallel == "none") {
      out <- apply(particles.PF.clean, model.FUN, MARGIN = 1)#, ...)

    }else if ((parallel == "parallel") | (parallel == "parallelWin")) {
      out <- parallel::parRapply(cl = cl, x = particles.PF.clean, FUN = model.FUN)#, ...)
      
    }

  }

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



  nvar <- length(out[[1]][["sim"]])

  dates.sim.list <- vector(mode = "list", length = nvar)

  for(i in 1:nvar){
    dates.sim.list[[i]] <- as.Date(time(out[[1]][["sim"]][[i]]))
  }
  
  list.matrix.num.sim <- vector(mode = "list", length = nvar)
  list.var <- list.matrix.num.sim

  # Initialising modelout matrixes

  for(i in 1:nvar){
    list.matrix.num.sim[[i]] <-matrix(NA, ncol = length(out), nrow = length(out[[1]][["sim"]][[i]]))
    colnames(list.matrix.num.sim[[i]]) <- paste0("sim_", id.PF.select)
  }


  # Initialising objs matrixes
  matrix.objs.num <-matrix(NA, ncol = length(out[[1]][["Objs"]]), nrow = length(out))
  colnames(matrix.objs.num) <- colnames(A)
  rownames(matrix.objs.num) <- id.PF.select

  if(write2disk){
    list.matrix.formatC.sim <- list.matrix.num.sim
    matrix.objs.formatC <- matrix.objs.num
  }

  # filling matrixes
  
  for(i in 1:length(out)){

    # filling modelout matrixes
    for(j in 1:nvar){
      list.matrix.num.sim[[j]][,i] <- coredata(out[[i]][["sim"]][[j]])
    }
    

    # filling objs matrixes
    matrix.objs.num[i,] <- out[[i]][["Objs"]]
  }

  for(i in 1:nvar){
    list.var[[i]] <- zoo(x = list.matrix.num.sim[[i]], order.by = dates.sim.list[[i]])
  }

  df.objs <- data.frame(Sim = id.PF.select, matrix.objs.num)



  if(write2disk){

    for(i in 1:length(out)){

      # filling modelout matrixes
      for(j in 1:nvar){
        list.matrix.formatC.sim[[j]][,i] <- formatC(coredata(out[[i]][["sim"]][[j]]), format="E", digits =digits, flag=" ") #cambiar
      }

      # filling objs matrixes
      matrix.objs.formatC[i,] <- formatC(out[[i]][["Objs"]], format="E", digits =digits, flag=" ")
    }

    for(i in 1:nvar){
      df.sim <- data.frame(Date_Sim = dates.sim.list[[i]], list.matrix.formatC.sim[[i]])
      write.table(df.sim, paste0(drty.out, "/", analysis.period, "/hydro_var",i,"_from_FilledPOF.txt"), row.names = FALSE, quote = FALSE)
    }

    df.objs.formatC <- data.frame(Sim = id.PF.select, matrix.objs.formatC)

    write.table(df.objs.formatC, 
                paste0(drty.out, "/", analysis.period, "/hydro_FilledPOF.txt"), 
                row.names = FALSE, quote = FALSE) 

  }


  ####

  #PArticles form filtered pareto front

  df.particles <- data.frame(id.PF.select, matrix(NA, ncol = ncol(particles.PF.select[,-c(1,2)]), 
                                                  nrow = nrow(particles.PF.select[,-c(1,2)])))
  colnames(df.particles) <- c("Sim", colnames(particles.PF.select[,-c(1,2)]))

  if(write2disk){
    df.particles.formatC <- df.particles
  }

  for(i in (obj.dim+1):ncol(particles.PF.select[,-c(1,2)])){
    df.particles[,i+1] <- particles.PF.select[,i+2]
  }

  df.particles[,1:(obj.dim+1)] <- df.objs

  if(write2disk){
    for(i in (obj.dim+1):ncol(particles.PF.select[,-c(1,2)])){
      df.particles.formatC[,i+1] <- formatC(particles.PF.select[,i+2], format="E", digits =digits, flag=" ")
    }
    df.particles.formatC[,1:(obj.dim+1)] <- df.objs.formatC
    write.table(df.particles.formatC, paste0(drty.out, "/", analysis.period, "/hydro_ParticlesFilledPOF.txt"), row.names = FALSE, quote = FALSE)
  }
  
  
  #--------
  # The best compromise solution must be found in the Pareto Optimum Front obtained in calibration...
  # ... hence matrix.objs.num.cal


  if(any(is.na(df.obj.thr))){
    matrix.upper <- matrix(apply(matrix.objs.num.cal,2,max), ncol = ncol(matrix.objs.num.cal), nrow = nrow(matrix.objs.num.cal), byrow = TRUE)
    matrix.lower <- matrix(apply(matrix.objs.num.cal,2,min), ncol = ncol(matrix.objs.num.cal), nrow = nrow(matrix.objs.num.cal), byrow = TRUE)

    matrix.objs.num.cal.norm <- (matrix.objs.num.cal - matrix.lower)/(matrix.upper - matrix.lower)

    utopia <- matrix(rep(apply(matrix.objs.num.cal.norm, MARGIN = 2, FUN = match.fun(MaxMin)), nrow(matrix.objs.num.cal.norm)),
                     ncol = ncol(matrix.objs.num.cal.norm), nrow = nrow(matrix.objs.num.cal.norm), byrow = TRUE)

  }else{
    matrix.upper <- matrix(as.numeric(apply(df.obj.thr,2,max)), ncol = ncol(matrix.objs.num.cal), nrow = nrow(matrix.objs.num.cal), byrow = TRUE)
    matrix.lower <- matrix(as.numeric(apply(df.obj.thr,2,min)), ncol = ncol(matrix.objs.num.cal), nrow = nrow(matrix.objs.num.cal), byrow = TRUE)

    matrix.objs.num.cal.norm <- (matrix.objs.num.cal - matrix.lower)/(matrix.upper - matrix.lower)

    utopia <- matrix(ifelse(MaxMin == "max", 1, 0),
                     ncol = ncol(matrix.objs.num.cal.norm), nrow = nrow(matrix.objs.num.cal.norm), byrow = TRUE)
  }


  

  
  distance <- sqrt(apply((utopia - matrix.objs.num.cal.norm)^2, MARGIN = 1, FUN = sum))
  
  index.min <- which.min(distance)
  id.sim.min <- paste0("sim_",id.PF.select[index.min])
  #--------
  
  df.bcs.sim.list <- vector(mode = "list", length(nvar))


  for(i in 1:nvar){
    df.bcs.sim.list[[i]] <- data.frame(Dates = dates.sim.list[[i]], rep(NA, times = length(dates.sim.list[[i]])))
    colnames(df.bcs.sim.list[[i]]) <- c("Date", paste0("var_", i))
  }


  if(write2disk){
    df.bcs.sim.formatC.list <- df.bcs.sim.list
  }

  for(i in 1:nvar){
    df.bcs.sim.list[[i]][,2] <- list.matrix.num.sim[[i]][,id.sim.min]
  }


  zoo.bcs.sim.list <- vector(mode = "list", length = nvar)

  for(i in 1:nvar){
    zoo.bcs.sim.list[[i]] <- zoo(x = df.bcs.sim.list[[i]][,2], order.by = as.Date(df.bcs.sim.list[[i]][,1]))
  }

  df.bcs.particle <- df.particles[index.min,]

  if(write2disk){

    for(i in 1:nvar){
      df.bcs.sim.formatC.list[[i]][,2] <- formatC(list.matrix.num.sim[[i]][,id.sim.min], format="E", digits =digits, flag=" ")
    }

    for(i in 1:nvar){
      write.table(df.bcs.sim.formatC.list[[i]], paste0(drty.out, "/", analysis.period, "/hydro_var",i,"_Best_CS_ModelOut.txt"), row.names = FALSE, quote = FALSE)
    }
    
    df.bcs.particle.formatC <- df.particles.formatC[index.min,]
    write.table(df.bcs.particle.formatC, paste0(drty.out, "/", analysis.period, "/hydro_Best_CS_Particle.txt"), row.names = FALSE, quote = FALSE)

  }
  
  index.best.particular.obj <- apply(matrix.objs.num, MARGIN = 2, FUN = match.fun(paste0("which.",MaxMin)))
  id.sim.best.particular.obj <- paste0("sim_",id.PF.select[index.best.particular.obj])

  list.best.obj.sim <- list(mode = "list", length = length(index.best.particular.obj))
  list.best.obj.particle <- list(mode = "list", length = length(index.best.particular.obj))
  
  for(j in 1:length(index.best.particular.obj)){

    df.best.obj.list <- vector(mode = "list", length = nvar)

    for(i in 1:nvar){
      df.best.obj.list[[i]] <- data.frame(Dates = dates.sim.list[[i]], rep(NA, times = length(dates.sim.list[[i]])))
      colnames(df.best.obj.list[[i]]) <- c("Date", paste0("var_", i))
    }

    if(write2disk){
      df.best.obj.formatC.list <- df.best.obj.list
    }
    
    for(i in 1:nvar){
      df.best.obj.list[[i]][,2] <- list.matrix.num.sim[[i]][,id.sim.best.particular.obj[j]]
    }


    list.best.obj.sim[[j]] <- vector(mode = "list", length = nvar)

    for(i in 1:nvar){
      list.best.obj.sim[[j]][[i]] <- zoo(x = df.best.obj.list[[i]][,2], order.by = as.Date(df.best.obj.list[[i]][,1]))
    }

    list.best.obj.particle[[j]] <- df.particles[index.best.particular.obj[j],]

    if(write2disk){

      for(i in 1:nvar){
        df.best.obj.formatC.list[[i]][,2] <- formatC(list.matrix.num.sim[[i]][,id.sim.best.particular.obj[j]], format="E", digits =digits, flag=" ")
      }

      for(i in 1:nvar){
        write.table(df.best.obj.formatC.list[[i]], paste0(drty.out, "/", analysis.period, "/hydro_var",i,"_Best_Obj",j,"_ModelOut.txt"), row.names = FALSE, quote = FALSE)
      }

      write.table(df.particles.formatC[index.best.particular.obj[j],], paste0(drty.out, "/", analysis.period, "/hydro_Best_Obj",j,"_Particle.txt"), row.names = FALSE, quote = FALSE)

    }
    
  }


  MOPSOResults <- list("ParetoFront" = rep.history, 
                       "ParticlesParetoFront" = pos.history,
                       "ObjsNames" = df.obj.names,
                       "MaxMin" = df.minmax
                       )

  hydroDetails <- list("Obs" = list.obs, # hyd
                       "Dimensions" = df.dimensions, # hyd
                       "NamesAndUnitsVars" = df.vars, # hyd
                       "WarmUp" = df.warmup, # hyd
                       "DatesCal" = df.dates.cal # hyd
                       )

  hydroResults <- list("ParticlesFull" = df.particles.full,
                       "FilledPOF" = df.objs,
                       "ParticlesFilledPOF" = df.particles,
                       "ModelOut" = list.var,
                       "ParticleBestCS" = df.bcs.particle,
                       "ModelOutBestCS" = zoo.bcs.sim.list,
                       "ParticleBestObjs" = list.best.obj.particle,
                       "ModelOutBestObjs" = list.best.obj.sim,
                       "AnalysisPeriod" = analysis.period,
                       "DigitsDom" = digits.dom,
                       "ObjsNames" = df.obj.names,
                       "MaxMin" = df.minmax,
                       "Obs" = list.obs,
                       "Dimensions" = df.dimensions,
                       "NamesAndUnitsVars" = df.vars,
                       "WarmUp" = df.warmup,
                       "DatesCal" = df.dates.cal
                       )

  out <- list(MOPSOResults, hydroDetails, hydroResults)
  names(out) <- c("MOPSOResults", "hydroDetails", "hydroResults")

  return(out)
} # END



