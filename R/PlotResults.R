PlotResults <- function(Results,
                        do.png = FALSE,
                        main = "study case #1",
                        drty.out = "MOPSO.out"){

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if(!is.null(Results[["hydroResults"]])){
    Results <- Results[["hydroResults"]]
  }


  analysis.period <- Results[["AnalysisPeriod"]]

  if(do.png){

    if(!dir.exists(paste0(drty.out, "/", analysis.period, "/png.out"))){
      dir.create(paste0(drty.out, "/", analysis.period, "/png.out"), recursive = TRUE)
    }
  }

 
  nobjs <- Results[["Dimensions"]][1,1]
  nvars <- Results[["Dimensions"]][1,2]
  MaxMin <- Results[["MaxMin"]][1,1]

  digits.dom <- Results[["DigitsDom"]]
  
  obj.names <- as.character(Results[["ObjsNames"]])
  var.names <- as.character(Results[["NamesAndUnitsVars"]])
  
  parameter.set.pof <- Results[["ParticlesFilledPOF"]][,-c(1:(nobjs+1))]
  obj.set.pof <- Results[["ParticlesFilledPOF"]][,c(2:(nobjs+1))]

  parameter.set.sub <- Results[["ParticlesFull"]][,-c(1:(nobjs+1))]
  obj.set.sub <- Results[["ParticlesFull"]][,c(2:(nobjs+1))]
  
  objs <- Results[["FilledPOF"]][,-1]
  
  raw.some.particles <- matrix(NA, ncol = ncol(Results[["ParticleBestCS"]]), nrow = 1 + length(Results[["ParticleBestObjs"]]))
  colnames(raw.some.particles) <- colnames(Results[["ParticleBestCS"]])
  raw.some.particles[1,] <- as.numeric(Results[["ParticleBestCS"]][1,])
  
  for(i in 1:length(Results[["ParticleBestObjs"]])){
    raw.some.particles[i+1,] <- as.numeric(Results[["ParticleBestObjs"]][[i]][1,])
  }

  some.particles <- raw.some.particles[,-c(1:(nobjs+1))]
  some.objs <- raw.some.particles[,c(2:(nobjs+1))]
  
  model.out <- Results[["ModelOut"]]

  dates <- as.Date(time(model.out[[1]]))
  
  model.out.bcs <- Results[["ModelOutBestCS"]]
  
  obs.var <- Results[["Obs"]]

  if(!is.null(Results[["WarmUp"]])){
    warmup <- as.Date(Results[["WarmUp"]][,1])
  }else{
    warmup <- NULL
  }

  dates.cal <- as.Date(Results[["DatesCal"]][,1])




  nparam <- ncol(parameter.set.pof)
  
  # Parameters on Pareto Front =====================================================================================
  
  if(do.png){
    png(filename=paste0(drty.out, "/", analysis.period, "/png.out/Parameters_on_Pareto_Optimal_Front.png"), width = 3840, height = 2160, res = 280)
  }
  
  nplots <- nparam + 1
  
  nrow.lay <- floor(sqrt(nplots))
  ncol.lay <- floor(sqrt(nplots))+1
  
  ncol.lay <- ifelse(nrow.lay*ncol.lay<nplots, ncol.lay + 1, ncol.lay)
  
  par(mfrow = c(nrow.lay, ncol.lay), oma = c(0, 0, 2, 0))
  
  ###
  
  names.sol <- c("Best Compromise Solution", paste0("Best ",obj.names))
  
  colors.sol <- rep("", length(nobjs))

  
  samp.colors1 <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C",
                   "#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#B15928",
                   "#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462",
                   "#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F")
  if(nparam<=24){
    colors.sol1 <- samp.colors1[1:nparam]
  }else{
    colors.sol1 <- c(samp.colors1, sample(samp.colors1, size = nparam - 24, replace = TRUE))
  }
  
  
  samp.colors2 <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00",
                    "#FFFF33","#A65628","#F781BF","#999999")
  if(nrow(some.particles)<=9){
    colors.sol2 <- samp.colors2[1:nrow(some.particles)]
  }else{
    colors.sol2 <- c(samp.colors2, sample(samp.colors2, size = nrow(some.particles) - 9, replace = TRUE))
  }
  
  ltype <- sample(c("dashed", "dotted", "dotdash", "longdash", "twodash"), size = nrow(some.particles), replace = TRUE)
  
  for(i in 1:nparam){
    boxplot(parameter.set.pof[,i], main = colnames(parameter.set.pof)[i],
            col = colors.sol1[i])
    
    for(j in 1:nrow(some.particles)){
      abline(h = some.particles[j,i], col = colors.sol2[j], lty = ltype[j], lwd = 2)
    }
    
    
  }
  
  plot(c(0,1),c(0,1),type = "n", axes = FALSE, xlab = "", ylab = "")
  legend(x = 0.5, y = 0.5,xjust = 0.5, yjust = 0.5,
         legend = names.sol, 
         col = colors.sol2, 
         lty = ltype,
         lwd = 2,
         bty = "n", 
         pt.cex = 1, 
         cex = 1, 
         text.col = "black", 
         horiz = F , 
         inset = c(0.1, 0.1))
  
  mtext("Parameters on Pareto Front", outer = TRUE, cex = 1.5, font = 2)
  
  if(do.png){
    dev.off()
  }
  
  
  # Parameter Dottyplots ==================================================================================
  
  if(analysis.period == "calibration"){
  
    for(j in 1:nobjs){

      if(do.png){
        png(filename=paste0(drty.out, "/", analysis.period, "/png.out/Parameter_Dottyplots_Obj",j,".png"), width = 3840, height = 2160, res = 280)
      }else{
        dev.new()
      }

      nplots <- nparam + 1
      
      nrow.lay <- floor(sqrt(nplots))
      ncol.lay <- floor(sqrt(nplots))+1
      
      ncol.lay <- ifelse(nrow.lay*ncol.lay<nplots, ncol.lay + 1, ncol.lay)
      
      par(mfrow = c(nrow.lay, ncol.lay), oma = c(0, 0, 2, 0))
      
      ###
      
      
      
      for(i in 1:nparam){
        plot(x = parameter.set.sub[,i], y = obj.set.sub[,j], 
             xlab = colnames(parameter.set.sub)[i], ylab = obj.names[j], 
             main = colnames(parameter.set.sub)[i], col = "black")
        points(x = parameter.set.pof[,i], y = obj.set.pof[,j], col = "red")
      }


      plot(c(0,1),c(0,1),type = "n", axes = FALSE, xlab = "", ylab = "")
      legend(x = 0.5, y = 0.5,xjust = 0.5, yjust = 0.5,
             legend = c("Pareto-optimal solutions","Sub-optimal solutions"), 
             col = c("red", "black"), 
             lty = 0,
             pch = c(1,1),
             lwd = 2,
             bty = "n", 
             pt.cex = 1, 
             cex = 1, 
             text.col = "black", 
             horiz = FALSE, 
             inset = c(0.1, 0.1))
        
      
      mtext(paste0("Parameters (Obj ",j,": ", obj.names[j],")"), outer = TRUE, cex = 1.5, font = 2)
      
      if(do.png){
        dev.off()
      }
    }
  
  }

  # Parameters Dottyplots ==================================================================================
  
  
  for(j in 1:nobjs){

    if(do.png){
      png(filename=paste0(drty.out, "/", analysis.period, "/png.out/Parameter_Dottyplots_just_POF_Obj",j,".png"), width = 3840, height = 2160, res = 280)
    }else{
      dev.new()
    }

    nplots <- nparam + 1
    
    nrow.lay <- floor(sqrt(nplots))
    ncol.lay <- floor(sqrt(nplots))+1
    
    ncol.lay <- ifelse(nrow.lay*ncol.lay<nplots, ncol.lay + 1, ncol.lay)
    
    par(mfrow = c(nrow.lay, ncol.lay), oma = c(0, 0, 2, 0))
    
    ###
    
    
    
    for(i in 1:nparam){
      plot(x = parameter.set.pof[,i], obj.set.pof[,j], xlab = colnames(parameter.set.pof)[i],
           ylab = obj.names[j],
           main = colnames(parameter.set.pof)[i], col = "red")
    }
    
    plot(c(0,1),c(0,1),type = "n", axes = FALSE, xlab = "", ylab = "")
    legend(x = 0.5, y = 0.5,xjust = 0.5, yjust = 0.5,
           legend = "Pareto-optimal solutions", 
           col = "red", 
           lty = 0,
           pch = 1,
           lwd = 2,
           bty = "n", 
           pt.cex = 1, 
           cex = 1, 
           text.col = "black", 
           horiz = FALSE, 
           inset = c(0.1, 0.1))
    
    mtext(paste0("Parameters on Pareto Front (Obj ",j,": ", obj.names[j],")"), outer = TRUE, cex = 1.5, font = 2)
    
    if(do.png){
      dev.off()
    }
  }
 
  
  # Time series comparison ==================================================================================
  
  nplots <- nvars

  if(!is.null(warmup)){
    flag.warmup <- TRUE
  }else{
    flag.warmup <- FALSE
    warning("Warm-up not specified")
  }

  if(analysis.period == "verification"){
    flag.cal <- TRUE
  }else{
    flag.cal <- FALSE
  }

  # Model Out =====================================================================================

  if(do.png){
    png(filename=paste0(drty.out, "/", analysis.period, "/png.out/ModelOut_from_Pareto_Optimal_Front.png"), width = 3840, height = nplots*1620, res = 280)
  }
  
    
  par(mfrow = c(nplots, 1), oma = c(0, 0, 0, 0))

  
  for(i in 1:length(model.out)){
    
    var.name <- Results[["NamesAndUnitsVars"]][i]
    var.unit <- Results[["NamesAndUnitsVars"]][nvars+i]
    
    varmin <- apply(coredata(model.out[[i]]), MARGIN = 1, FUN = min)
    varmax <- apply(coredata(model.out[[i]]), MARGIN = 1, FUN = max)
    
    ylim <- c(min(c(varmin, coredata(obs.var[[i]])), na.rm = TRUE),
              max(c(varmax, coredata(obs.var[[i]])), na.rm = TRUE))

    xlim <- c(min(dates), max(dates)) 

    plot(zoo(varmax, dates), type = "n", xaxt = "n", xlab = "Date", ylab = paste0(var.name, " [",var.unit,"]"),
         main = "", ylim = ylim, xlim = xlim)
    title(main = paste0(var.name, " in ", main), adj = 0)
    if(flag.warmup){
      polygon(x = c(warmup, rev(warmup)), y = c(rep(ylim[1]-0.05*(ylim[2]-ylim[1]), length(warmup)), rep(ylim[2]+1.05*(ylim[2]-ylim[1]), length(warmup))), border = NA, col = "#DBDBDB")
    }

    if(flag.cal){
      polygon(x = c(dates.cal, rev(dates.cal)), y = c(rep(ylim[1]-0.05*(ylim[2]-ylim[1]), length(dates.cal)), rep(ylim[2]+1.05*(ylim[2]-ylim[1]), length(dates.cal))), border = NA, col = "#DCEDFF")
    }

    grid()
    if(flag.warmup){
      text(x = xlim[1], y = ylim[2], adj = c(1,1), label = "Warm-Up", srt = 90, col = "#666666", font = 2)
    }
    if(flag.cal){
      text(x = dates.cal[1], y = ylim[2], adj = c(0,1), label = "Calibration", srt = 0, col = "#0072EA", font = 2)
    }
    box()
    drawTimeAxis(zoo(varmax, dates))
    PlotBandsOnly(lband = zoo(varmin, dates), uband = zoo(varmax, dates), bands.col = "#e800d1")
    lines(zoo(varmin, dates), col = "#e800d1", lwd = 0.5)
    lines(zoo(varmax, dates), col = "#e800d1", lwd = 0.5)
    lines(model.out.bcs[[i]], col = "#004fcf", lwd = 0.5)
    
    par(xpd = TRUE)

    legend(x = xlim[1] + 0.80*(xlim[2] - xlim[1]),  xjust = 0,
           y = ylim[2] + 0.05*(ylim[2] - ylim[1]), yjust = 0,
           legend = c("Best Compromise Solution", "Paretro Front bands"), 
           col = c("#004fcf", "#e800d1"), 
           lty = c(1,0),
           lwd = 0.5,
           pch = c(0,15),
           bty = "n", 
           pt.cex = c(0,1), 
           cex = 1, 
           text.col = "black", 
           horiz = FALSE, 
           inset = c(0.1, 0.1))

    par(xpd = FALSE)
  }
  
  if(do.png){
    dev.off()
  }
  
  # Obs vs Model Out ===============================================================================
  
  if(do.png){
    png(filename=paste0(drty.out, "/", analysis.period, "/png.out/ModelOut_from_Pareto_Optimal_Front_vs_Obs.png"), width = 3840, height = nplots*1620, res = 280)
  }
  

  par(mfrow = c(nplots, 1), oma = c(0, 0, 0, 0))


  
  for(i in 1:length(model.out)){
    
    var.name <- Results[["NamesAndUnitsVars"]][i]
    var.unit <- Results[["NamesAndUnitsVars"]][nvars+i]
    
    varmin <- apply(coredata(model.out[[i]]), MARGIN = 1, FUN = min)
    varmax <- apply(coredata(model.out[[i]]), MARGIN = 1, FUN = max)

    ylim <- c(min(c(varmin, coredata(obs.var[[i]])), na.rm = TRUE),
              max(c(varmax, coredata(obs.var[[i]])), na.rm = TRUE))

    xlim <- c(min(dates), max(dates)) 
    
    plot(zoo(varmax, dates), type = "n", xaxt = "n", xlab = "Date", ylab = paste0(var.name, " [",var.unit,"]"),
         main = "", ylim = ylim, xlim = xlim)
    title(main = paste0(var.name, " in ", main), adj = 0)
    if(flag.warmup){
      polygon(x = c(warmup, rev(warmup)), y = c(rep(ylim[1]-0.05*(ylim[2]-ylim[1]), length(warmup)), rep(ylim[2]+1.05*(ylim[2]-ylim[1]), length(warmup))), border = NA, col = "#DBDBDB")
    }
    if(flag.cal){
      polygon(x = c(dates.cal, rev(dates.cal)), y = c(rep(ylim[1]-0.05*(ylim[2]-ylim[1]), length(dates.cal)), rep(ylim[2]+1.05*(ylim[2]-ylim[1]), length(dates.cal))), border = NA, col = "#DCEDFF")
    }
    grid()
    if(flag.warmup){
      text(x = xlim[1], y = ylim[2], adj = c(1,1), label = "Warm-Up", srt = 90, col = "#666666", font = 2)
    }
    if(flag.cal){
      text(x = dates.cal[1], y = ylim[2], adj = c(0,1), label = "Calibration", srt = 0, col = "#0072EA", font = 2)
    }
    box()
    drawTimeAxis(zoo(varmax, dates))
    PlotBandsOnly(lband = zoo(varmin, dates), uband = zoo(varmax, dates), bands.col = "#e800d1")
    lines(zoo(varmin, dates), col = "#e800d1", lwd = 0.5)
    lines(zoo(varmax, dates), col = "#e800d1", lwd = 0.5)
    lines(obs.var[[i]], col = "black", lwd = 0.5)

    par(xpd = TRUE)
    
    legend(x = xlim[1] + 0.80*(xlim[2] - xlim[1]),  xjust = 0,
           y = ylim[2] + 0.05*(ylim[2] - ylim[1]), yjust = 0,
           legend = c("Observation", "Paretro Front bands"), 
           col = c("black", "#e800d1"), 
           lty = c(1,0),
           lwd = 0.5,
           pch = c(0,15),
           bty = "n",
           pt.cex = c(0,1), 
           cex = 1, 
           text.col = "black", 
           horiz = FALSE, 
           inset = c(0.1, 0.1))

    par(xpd = FALSE)
  }
  
  if(do.png){
    dev.off()
  }



  # Model Out BCS vs Obs =====================================================================================

  if(do.png){
    png(filename=paste0(drty.out, "/", analysis.period, "/png.out/ModelOut_BCS_from_Pareto_Optimal_Front_vs_Obs.png"), width = 3840, height = nplots*1620, res = 280)
  }

  SimVsObs(Sim = model.out.bcs,
          Obs = obs.var,
          Objs.names = obj.names,
          Objs.values = as.numeric(Results[["ParticleBestCS"]][1,2:(nobjs+1)]),
          var.names = Results[["NamesAndUnitsVars"]][1:nvars],
          var.units = Results[["NamesAndUnitsVars"]][(nvars+1):(2*nvars)],
          legend.sim = "Best Compromise Solution",
          legend.obs = "Observed",
          full.period = dates,
          cal.period = dates.cal,
          warmup.period = warmup,
          main = main,
          analysis.period = analysis.period,
          digits.round = digits.dom)
  
  if(do.png){
    dev.off()
  }
  
  # Pareto Fronts =====================================================================================

  if(analysis.period == "calibration"){
    main_2d <- "Pareto Optimal Front in calibration period"
    col_o <- "#004fcf"
  }else if(analysis.period == "verification"){
    main_2d <- "Degradation of the Pareto Optimal Front in verification period"
    col_o <- "#AF2121"
  }
  
  if(do.png){
    png(filename=paste0(drty.out, "/", analysis.period, "/png.out/Solutions_2D_Compromise.png"), width = 3840, height = 3840, res = 400)
  }else{
    dev.new()
  }


  egrid <- expand.grid(1:nobjs,1:nobjs)

  flag_1 <- !(apply(egrid,1,diff) == 0)
  flag_2 <- !duplicated(apply(t(apply(egrid,1,sort)),1,paste,collapse = "_"))


  comb <- egrid[flag_1 & flag_2,]

  nplots <- nrow(comb)
  
  nrow.lay <- floor(sqrt(nplots))
  ncol.lay <- floor(sqrt(nplots))
  
  ncol.lay <- ifelse(nrow.lay*ncol.lay<nplots, ncol.lay, ncol.lay)
  
  par(mfrow = c(nrow.lay, ncol.lay), oma = c(0, 0, 2, 0))


  for(i in 1:nplots){

    aa <- comb[i,1]
    bb <- comb[i,2]

    plot(obj.set.pof[,aa], obj.set.pof[,bb], col = col_o, xlab = obj.names[aa], ylab = obj.names[bb], main = paste0(obj.names[aa], " v ", obj.names[bb]))

  }
  
  
  if(do.png){
    dev.off()
  }
  
}





SimVsObs <- function(Sim,
                     Obs,
                     Objs.values,
                     Objs.names,
                     var.names, 
                     var.units,
                     legend.sim = "Simulated", 
                     legend.obs = "Observed", 
                     cal.period,
                     warmup.period,
                     full.period,
                     main = "study case #1",
                     analysis.period,
                     digits.round = 8){

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  nobjs <- length(Objs.values)
  nvars <- length(Obs)

  par(mfrow = c(nvars, 1), oma = c(0, 0, 0, 0))

  objs.text <- apply(data.frame(Objs.names, rep(": ", nobjs), 
                 round(Objs.values, digits.round)), 
                 MARGIN = 1, FUN = paste, collapse = "")

  objs.text.just <- paste0(analysis.period, " GoFs\n", paste(format(objs.text, width = max(sapply(objs.text, FUN = nchar)), justify = "right"), collapse = "\n"))

  for(i in 1:nvars){

    var.sim <- Sim[[i]]
    var.obs <- Obs[[i]] 

    var.name <- var.names[i]
    var.unit <- var.units[i]

    if(!is.null(warmup.period)){
      flag.warmup <- TRUE
    }else{
      flag.warmup <- FALSE
      warning("Warm-up not specified")
    }



    if(analysis.period == "verification" | sum(length(full.period)) != sum(length(cal.period) + length(warmup.period))){
      flag.cal <- TRUE
    }else{
      flag.cal <- FALSE
    }


    ylim <- c(min(c(coredata(var.sim), coredata(var.obs)), na.rm = TRUE),
              max(c(coredata(var.sim), coredata(var.obs)), na.rm = TRUE))

    xlim <- c(min(full.period), max(full.period)) 

    plot(var.sim, type = "n", xaxt = "n", xlab = "Date", ylab = paste0(var.name, " [",var.unit,"]"),
         main = "", ylim = ylim, xlim = xlim)
    title(main = paste0(var.name, " in ", main), adj = 0)
    if(flag.warmup){
      polygon(x = c(warmup.period, rev(warmup.period)), y = c(rep(ylim[1]-0.05*(ylim[2]-ylim[1]), length(warmup.period)), rep(ylim[2]+1.05*(ylim[2]-ylim[1]), length(warmup.period))), border = NA, col = "#DBDBDB")
    }
    if(flag.cal){
      polygon(x = c(cal.period, rev(cal.period)), y = c(rep(ylim[1]-0.05*(ylim[2]-ylim[1]), length(cal.period)), rep(ylim[2]+1.05*(ylim[2]-ylim[1]), length(cal.period))), border = NA, col = "#DCEDFF")
    }
    grid()
    if(flag.warmup){
      text(x = xlim[1], y = ylim[2], adj = c(1,1), label = "Warm-Up", srt = 90, col = "#666666", font = 2)
    }
    if(flag.cal){
      text(x = cal.period[1], y = ylim[2], adj = c(0,1), label = "Omitted", srt = 0, col = "#0072EA", font = 2)
    }

    text(x = xlim[2], y = ylim[2], label = objs.text.just, adj = c(1,1), cex = 0.8)
    box()
    drawTimeAxis(var.sim)
    lines(var.obs, col = "black",type = "o", lwd = 1, pch = 16, cex = 0.4)
    lines(var.sim, col = "#004fcf", lwd = 1)

    par(xpd = TRUE)

    legend(x = xlim[1] + 0.80*(xlim[2] - xlim[1]),  xjust = 0,
           y = ylim[2] + 0.05*(ylim[2] - ylim[1]), yjust = 0,
           legend = c(legend.obs, legend.sim), 
           col = c("black", "#004fcf"), 
           lty = c(1,1),
           lwd = 1,
           pch = c(16,0),
           bty = "n", 
           pt.cex = c(0.4,0), 
           cex = 1, 
           text.col = "black", 
           horiz = FALSE, 
           inset = c(0.1, 0.1))

    par(xpd = FALSE)
    
  }

}


