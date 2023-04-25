# File PSO_v2013.R
# Part of the hydroPSO R package, https://github.com/hzambran/hydroPSO
#                                 http://cran.r-project.org/web/packages/hydroPSO
#                                 http://www.rforge.net/hydroPSO/
# Copyright 2010-2020 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
##                          RandomBoundedMatrix                             ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 2008                                                               ##
# Updates: 23-Nov-2010                                                        ##
#          20-Sep-2012 ; 29-Oct-2012                                          ##
################################################################################
# Purpose  : To create a matrix randomly generated, with a bounded uniform distribution

# 'npart'   : number of particles in the swarm
# 'X.MinMax': Matrix of 'n' rows and 2 columns, 
#             the first column has the minimum values for each dimension, and
#             the second column has the maximum values for each dimension
RandomBoundedMatrix <- function(npart, x.MinMax) {
  
  # dimension of the solution space (number of parameters )
  n <- nrow(x.MinMax)
  
  lower <- matrix( rep(x.MinMax[,1], npart), nrow=npart, byrow=TRUE)
  upper <- matrix( rep(x.MinMax[,2], npart), nrow=npart, byrow=TRUE)
  
  # random initialization for all the particles, with a value in [0,1]
  X <- matrix(runif(n*npart, min=0, max=1), nrow=npart, ncol=n)
  
  # Transforming X into the real range defined by the user
  #X <- t( lower +  (upper - lower )*t(X) ) # when using vector instead of matrixes
  X <- lower + (upper-lower)*X  
  
} # 'RandomBoundedMatrix' end


################################################################################
##                        random Latin-Hypercube Sampling                     ##
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                         ##
# Created: 17-Dec-2010                                                        ##
# Updates: 20-Sep-2012  ; 29-Oct-2012                                         ##
#          07-Feb-2014                                                        ##
################################################################################
# Purpose  : Draws a Latin Hypercube Sample from a set of uniform distributions
#            for use in creating a Latin Hypercube Design
################################################################################
# Output   : An n by ndim Latin Hypercube Sample matrix with values uniformly 
#            distributed on 'ranges'
################################################################################

# 'n'      : number of strata used to divide each parameter range. 
#            For hydroPSO: 'n=npart'
# 'ranges' : Matrix of 'N' rows and 2 columns, (N is the number of parameters)
#            the first column has the minimum values for each dimension, and
#            the second column has the maximum values for each dimension
rLHS <- function(n, ranges) {
  
  # dimension of the solution space (number of parameters )
  ndim <- nrow(ranges)
  
  # number of particles
  npart <- n
  
  lower <- matrix( rep(ranges[,1], npart), nrow=npart, byrow=TRUE)
  upper <- matrix( rep(ranges[,2], npart), nrow=npart, byrow=TRUE)
  
  # LHS initialization for all the particles, with a value in [0,1]
  X <- randomLHS(n, ndim) # lhs::randomLHS
  
  X <- lower + (upper-lower)*X  
  
} # 'rLHS' end



################################################################################
##                        random Sobol Sampling                               ##
################################################################################
# Author: Mauricio Zambrano-Bigiarini & RMR                                   ##
# Created: 26-Dec-2020                                                        ##
################################################################################
# Purpose  : Draws a Sobol Sample from a set of uniform distributions         ##
#            for use in creating a Sobol Design                               ##
################################################################################
# Output   : An n by ndim Sobol Sample matrix with values uniformly           ##
#            distributed on 'ranges'                                          ##
################################################################################
#                                                                             ##
# 'n'      : number of strata used to divide each parameter range. 
#            For hydroPSO: 'n=npart'
# 'ranges' : Matrix of 'N' rows and 2 columns, (N is the number of parameters)
#            the first column has the minimum values for each dimension, and
#            the second column has the maximum values for each dimension
rSobol <- function(n, ranges) {
  
  # dimension of the solution space (number of parameters)
  
  ndim <- nrow(ranges)
  
  # number of particles
  npart <- n
  
  lower <- matrix( rep(ranges[,1], npart), nrow=npart, byrow=TRUE)
  upper <- matrix( rep(ranges[,2], npart), nrow=npart, byrow=TRUE)
  
  # Sobol initialization for all the particles, with a value in [0,1]
  X <- randtoolbox::sobol(n=n, dim=ndim, scrambling=3, seed = floor(runif(1, 1, 99999)))  
  
  # Transforming X into the real range defined by the user
  #X <- t( lower +  (upper - lower )*t(X) ) # when using vector instead of matrixes
  X <- lower + (upper-lower)*X  
  
} # 'rSobol' end



################################################################################
##                    VelocityBoundaryTreatment Function                    ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 2008                                                                #
# Updates:                                                                     #
################################################################################
# 'x' 		  : vector of 'n' parameters, corresponding to one particle
# 'n'             : dimension of the solution space (number of parameters)
# 'X.MinMax'      : string indicating if PSO have to find a minimum or a maximum 
#                   for the fitness function.
#                   valid values are: "min", "max"
# 'v'             : vector of 'n' velocities of each parameter, corresponding to 
#                   the current particle
# 'boundary.wall' : boundary treatment that is used to limit the sea
#                   the limits given by 'X.MinMax'.
#                   Valid values are: 'absorbing', 'reflecting' and 'invisible'

# Result          : vector of 'n' velocities, one for each parameter, 
#                   corresponding to the current particle
VelocityBoundaryTreatment <- function(v, vmax ) {	
  
  byd.vmax.pos <- which( abs(v) > vmax )
  if ( length(byd.vmax.pos) > 0 ) 
    v[byd.vmax.pos] <- sign(v[byd.vmax.pos])*abs(vmax[byd.vmax.pos])
  
  return(v)
  
} # 'VelocityBoundaryTreatment' end


################################################################################
##                  PositionIpdate Function           ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 2008                                                                #
# Updates: Nov-2011                                                            #
#          23-Sep-2012 ; 29-Oct-2012                                           #
################################################################################
# 'x'             : vector of 'n' parameters, corresponding to one particle
# 'X.MinMax'      : string indicating if PSO have to find a minimum or a maximum 
#                   for the fitness function.
#                   valid values are: "min", "max"
# 'v'             : vector of 'n' velocities of each parameter, corresponding to 
#                   the current particle
# 'boundary.wall' : boundary treatment that is used to limit the sea
#                   the limits given by 'X.MinMax'.
#                   Valid values are: 'absorbing', 'reflecting', 'invisible', 'damping'

# Result          : vector of 'n' velocities, one for each parameter, 
#                   corresponding to the current particle

# References:
# Robinson, J.; Rahmat-Samii, Y.; Particle swarm optimization in electromagnetics. 
# Antennas and Propagation, IEEE Transactions on , vol.52, no.2, pp. 397-407, 
# Feb. 2004. doi: 10.1109/TAP.2004.823969

# Huang, T.; Mohan, A.S.; , A hybrid boundary condition for robust particle
# swarm optimization. Antennas and Wireless Propagation Letters, IEEE , vol.4, 
# no., pp. 112-117, 2005. doi: 10.1109/LAWP.2005.846166
PositionIpdate <- function(x, v, x.MinMax, boundary.wall) {
  
  # Vector with the new positions of the current particle
  x.new <- x + v
  
  # By default the new velocity is assumed not to be limited
  v.new <- v
  
  # Minimum and maximum values for each dimension
  x.min <- x.MinMax[,1]
  x.max <- x.MinMax[,2]
  
  byd.min.pos <- which(x.new < x.min)
  if ( length(byd.min.pos) > 0) { 
    if ( boundary.wall == "absorbing2011") {     
      x.new[byd.min.pos] <- x.min[byd.min.pos]
      v.new[byd.min.pos] <- -0.5*v[byd.min.pos]      
    } else if ( boundary.wall == "absorbing2007") {     
      x.new[byd.min.pos] <- x.min[byd.min.pos]
      v.new[byd.min.pos] <- 0*v[byd.min.pos]      
    } else if ( boundary.wall == "reflecting") {    
      x.new[byd.min.pos] <- 2*x.min[byd.min.pos] - x.new[byd.min.pos] 
      v.new[byd.min.pos] <- -v[byd.min.pos]
    } else if ( boundary.wall == "invisible") {
      x.new[byd.min.pos] <- x[byd.min.pos]
      v.new[byd.min.pos] <- v[byd.min.pos]
    } else if ( boundary.wall == "damping") {
      L                  <- abs( x.min[byd.min.pos] - x.new[byd.min.pos] )
      x.new[byd.min.pos] <- x.min[byd.min.pos] + runif(1)*L
      v.new[byd.min.pos] <- -v[byd.min.pos]
    }# ELSE end
  } # IF end
  
  byd.max.pos <- which( x.new > x.max )
  if ( length(byd.max.pos) > 0 ) {	 
    if ( boundary.wall == "absorbing2011") { 
      x.new[byd.max.pos] <- x.max[byd.max.pos]
      v.new[byd.max.pos] <- -0.5*v[byd.max.pos] 
    } else if ( boundary.wall == "absorbing2007") { 
      x.new[byd.max.pos] <- x.max[byd.max.pos]
      v.new[byd.max.pos] <- 0*v[byd.max.pos] 
    } else if ( boundary.wall == "reflecting") {
      x.new[byd.max.pos] <- 2*x.max[byd.max.pos] - x.new[byd.max.pos] 
      v.new[byd.max.pos] <- -v[byd.max.pos]
    } else if ( boundary.wall == "invisible") {
      x.new[byd.max.pos] <- x[byd.max.pos]
      v.new[byd.max.pos] <- v[byd.max.pos]
    } else if ( boundary.wall == "damping") {
      L                  <- abs( x.new[byd.max.pos] - x.max[byd.max.pos])
      x.new[byd.max.pos] <- x.max[byd.max.pos] - runif(1)*L
      v.new[byd.max.pos] <- -v[byd.max.pos]
    }# ELSE end
  } # IF end
  
  out <- list(x.new=x.new, v.new=v.new)
  
} # 'PositionIpdate' end


################################################################################
##                         SimpleBoundaryTreatment                          ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Started: 2008                                                               ##
# Updates: Nov-2011                                                           ##
#          23-Sep-2012 ; 29-Oct-2012                                          ##
################################################################################
# 'x.new'         : Vector with the new positions of the current particle
# 'X.MinMax'      : string indicating if PSO have to find a minimum or a maximum 
#                   for the fitness function.
#                   valid values are: "min", "max"
# 'boundary.wall' : boundary treatment that is used to limit the sea
#                   the limits given by 'X.MinMax'.
#                   Valid values are: 'absorbing', 'reflecting', 'invisible', 'damping'

# Result          : vector of 'n' velocities, one for each parameter, 
#                   corresponding to the current particle

# References:
# Robinson, J.; Rahmat-Samii, Y.; Particle swarm optimization in electromagnetics. 
# Antennas and Propagation, IEEE Transactions on , vol.52, no.2, pp. 397-407, 
# Feb. 2004. doi: 10.1109/TAP.2004.823969

# Huang, T.; Mohan, A.S.; , A hybrid boundary condition for robust particle
# swarm optimization. Antennas and Wireless Propagation Letters, IEEE , vol.4, 
# no., pp. 112-117, 2005. doi: 10.1109/LAWP.2005.846166
SimpleBoundaryTreatment <- function(x.new, x.MinMax, boundary.wall) {
  
  # Minimum and maximum values for each dimension
  x.min <- x.MinMax[,1]
  x.max <- x.MinMax[,2]
  
  byd.min.pos <- which(x.new < x.min)
  if ( length(byd.min.pos) > 0) { 
    if ( boundary.wall == "absorbing2011") {     
      x.new[byd.min.pos] <- x.min[byd.min.pos]
    } else if ( boundary.wall == "absorbing2007") {     
      x.new[byd.min.pos] <- x.min[byd.min.pos]
    } else if ( boundary.wall == "reflecting") {    
      x.new[byd.min.pos] <- 2*x.min[byd.min.pos] - x.new[byd.min.pos] 
    } else if ( boundary.wall == "damping") {
      L                  <- abs( x.min[byd.min.pos] - x.new[byd.min.pos] )
      x.new[byd.min.pos] <- x.min[byd.min.pos] + runif(1)*L
    }# ELSE end
  } # IF end
  
  byd.max.pos <- which( x.new > x.max )
  if ( length(byd.max.pos) > 0 ) {	 
    if ( boundary.wall == "absorbing2011") { 
      x.new[byd.max.pos] <- x.max[byd.max.pos]
    } else if ( boundary.wall == "absorbing2007") { 
      x.new[byd.max.pos] <- x.max[byd.max.pos]
    } else if ( boundary.wall == "reflecting") {
      x.new[byd.max.pos] <- 2*x.max[byd.max.pos] - x.new[byd.max.pos] 
    } else if ( boundary.wall == "damping") {
      L                  <- abs( x.new[byd.max.pos] - x.max[byd.max.pos])
      x.new[byd.max.pos] <- x.max[byd.max.pos] - runif(1)*L
    }# ELSE end
  } # IF end
  
  out <- list(x.new=x.new)
  
} # 'PositionIpdate' end





################################################################################
#                            InitializateX                                     #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 23-Dec-2010                                                         #
# Updates: 24-Dec-2010                                                         #
#          28-Oct-2012                                                         #
################################################################################
# Purpose: Function for the initialization of the position and the velocities  # 
# of all the particles in the swarm                                            #
################################################################################
# -) npart     : number of particles
# -) X.MinMax  : Matrix with the minimum and maximum values for each dimension 
#                during the current iteration
#              -) Rows = 'n' (number of parameters)
#              -) Columns = 2, 
#                 First column has the minimum possible value for each parameter
#                 Second column has the maximum possible value for each parameter
# 'init.type' : character, indicating how to carry out the initialization 
#               of the position of all the particles in the swarm
#               valid values are in c('random', 'lhs') 
InitializateX <- function(npart, x.MinMax, x.ini.type) {
  
  # 'X' #
  # Matrix of unknown parameters. 
  # Rows = 'npart'; 
  # Columns = 'n' (Dimension of the Solution Space)
  # Random bounded values are assigned to each dimension
  if(x.ini.type=="lhs"){
    
    X <- rLHS(npart, x.MinMax)  
    
  }else if (x.ini.type=="Sobol"){
    
    X <- rSobol(npart, x.MinMax)
    
  }else {
    
    X <- RandomBoundedMatrix(npart, x.MinMax)
    
  }
  
  return(X)
  
} # InitializateX


################################################################################
#                            InitializateV                                     #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 24-Dec-2010                                                         #
# Updates: 24-Nov-2011                                                         #
#          17-Sep-2012 ; 28-Oct-2012                                           #
################################################################################
# Purpose: Function for the initialization of the position and the velocities  #
#          of all the particles in the swarm                                   #
################################################################################
# -) npart     : number of particles
# -) X.MinMax  : Matrix with the minimum and maximum values for each dimension 
#                during the current iteration
#              -) Rows = 'n' (number of parameters)
#              -) Columns = 2, 
#                 First column has the minimum possible value for each parameter
#                 Second column has the maximum possible value for each parameter
# 'v.ini'      : character, indicating how to carry out the initialization 
#                of the velocitites of all the particles in the swarm
#                valid values are in c('zero', 'random2007', 'lhs2007', 'random2011', 'lhs2011') 
InitializateV <- function(npart, x.MinMax, v.ini.type, Xini) {
  
  # Number of parameters
  n <- nrow(x.MinMax)
  
  # 'V' #
  # Matrix of velocities for each particle and iteration. 
  # Rows    = 'npart'; 
  # Columns = 'n' (Dimension of the solution space)
  # Random bounded values are assigned to each dimension
  
  if (v.ini.type %in% c("random2011", "lhs2011") ) {
    lower <- matrix( rep(x.MinMax[,1], npart), nrow=npart, byrow=TRUE)
    upper <- matrix( rep(x.MinMax[,2], npart), nrow=npart, byrow=TRUE)
    
    if ( v.ini.type=="random2011" ) {
      V <- matrix(runif(n*npart, min=as.vector(lower-Xini), max=as.vector(upper-Xini)), nrow=npart)
    } else if ( v.ini.type=="lhs2011" ) {
      # LHS initialization for all the particles, with a value in [0,1]
      V <- randomLHS(npart, n) 
      
      # Transforming V into the real range defined by SPSO-2011
      lower <- lower - Xini
      upper <- upper - Xini
      
      V <- lower + (upper-lower)*V  
    } # ELSE end 
  } else if ( v.ini.type=="random2007" ) {
    V <- ( RandomBoundedMatrix(npart, x.MinMax) - Xini ) / 2
  } else if ( v.ini.type=="lhs2007" ) {
    V <- ( rLHS(npart, x.MinMax) - Xini ) / 2
  } else if ( v.ini.type=="zero" ) {
    V <- matrix(0, ncol=n, nrow=npart, byrow=TRUE)    
  } # ELSE end
  
  return(V)
  
} # InitializateV




################################################################################
#                             hydromodEval                                    #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 13-Jan-2011                                                         #
# Updates: 18-Nov-2011                                                         #
#          06-Nov-2012 ; 07-Nov-2012 ; 08-Nov-2012                             #
################################################################################
# Function for evaluating the hydrological model for a single particle         #
################################################################################
### Started: 21-Jun-2011                                                       #
### Updates: 28-Jun-2011                                                       #
###          19-Jun-2012 ; 03-Jul-2012 ; 09-Jul-2012 ; 04-Dec-2012             #
################################################################################
hydromodEval <- function(part, Particles, iter, npart, maxit, 
                          REPORT, verbose, digits, 
                          model.FUN, model.FUN.args, 
                          parallel, ncores, part.dirs) {
  
  if ( iter/REPORT == floor(iter/REPORT) ) {
    if (verbose) message("================================================================================")
    if (verbose) message( "[Iter: ", format( iter, width=4, justify="left" ), "/", maxit, 
                          ". Particle: ", format( part, width=4, justify="left" ), "/", npart, 
                          ": Starting...]" )
    if (verbose) message("================================================================================")
  } # IF end
  if (parallel!="none")         
    model.FUN.args <- modifyList(model.FUN.args, list(model.drty=part.dirs[part]) ) 
  
  # Creating the R output
  nelements <- 2        
  out       <- vector("list", nelements)
  
  # Evaluating the hydrological model
  model.FUN.args <- modifyList(model.FUN.args, list(param.values=Particles[part,]) ) 
  hydromod.out   <- do.call(model.FUN, as.list(model.FUN.args)) 
  
  out[[1]] <- as.numeric(hydromod.out[["Objs"]])
  out[[2]] <- hydromod.out[["sim"]]
  
  # meaningful names
  names(out)[1:nelements] <- c("Objs", "sim") 
  
  if ( iter/REPORT == floor(iter/REPORT) ) {
    if (verbose) message("================================================================================")
    if (verbose) message( "[Iter: ", format( iter, width=4, justify="left" ), "/", maxit,  
                          ". Particle: ", format( part, width=4, justify="left" ), "/", npart,  
                          ". Finished !.   Objs: ", paste(format(hydromod.out[["Objs"]], scientific=TRUE, digits=digits), collapse = "   "), 
                          "]" )
    if (verbose) message("================================================================================")
    if (verbose) message("                                    |                                           ")  
    if (verbose) message("                                    |                                           ")    
  } # IF end
  
  return(out)
  
} # 'hydromodEval' END


