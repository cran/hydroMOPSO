
################################################################################
##                                NormalizeObjFun                            ##
################################################################################
# Author : Rodrigo Marinao Rivas                                              ##
################################################################################
# Created: 2020                                                               ##
# Updates:                                                                    ##
#                                                                             ##
################################################################################
# Description: 'The purpose of this function is to take the positions of the
#               particles within the OF-hyperspace, and normalize them for each
#               dimension (i.e. normalize for each objective function)'.
#
# Arguments:
# All: 'matrix' - Row bind of repository (Rep) and new Positions (Pop)
# Rep: 'matrix' - Repository
#
# Value:
# 'matrix'  -normalised particle position matrix

NormalizeObjFun <- function(All, Rep){


  NormalAll <- matrix(NA, nrow = nrow(All), ncol = ncol(All))
  for(i in 1:ncol(All)){
    
    if(!is.null(Rep)){
      Max <- max(Rep[,i]) # na.rm = TRUE not required
      Min <- min(Rep[,i]) # na.rm = TRUE not required
    }else if(is.null(Rep)){
      Max <- max(All[,i]) # na.rm = TRUE not required
      Min <- min(All[,i]) # na.rm = TRUE not required
    }
    
    if((Max - Min) != 0){
      NormalAll[,i] <- (All[,i] - Min)/(Max - Min)
    }else if((Max - Min) == 0){
      NormalAll[,i] <- sample(seq(0,1, length.out = nrow(NormalAll)), size = nrow(NormalAll))
    }

  }

  check_zero <- apply(NormalAll, MARGIN  = 1, FUN = sum)

  if(any(check_zero == 0)){
    NormalAll[check_zero == 0,] <- 1e-8
  }

  return(NormalAll)
}

# END NormalizeObjFun
################################################################################



################################################################################
##                           ShiftedDistanceNGB                             ##
################################################################################
# Author : Rodrigo Marinao Rivas                                              ##
################################################################################
# Created: 2020                                                               ##
# Updates:                                                                    ##
#                                                                             ##
################################################################################
# Description: 'The porpuse of this function is to calculate the shifted
#               Euclidian distance to the nearest neighbor, for the matrix of
#               normalised particle's position'
#
#
# Arguments:
# NOF: 'matrix' (even with one single column). Contain the normalised 
#      particle's position in the OF-hyperspace, i.e. the hyperspace of
#      Objective Function
#
# Value:
# 'matrix' - matrix of shifted Euclidian distance to the nearest neighbor
#

ShiftedDistanceNGB <- function(NOF){
  sde <- matrix(Inf, nrow = nrow(NOF), ncol = nrow(NOF))
  for(i in 1:nrow(NOF)){
    S.NOF <- pmax(NOF, matrix(NOF[i,], nrow = nrow(NOF), ncol = ncol(NOF), byrow = TRUE))
    for(j in 1:nrow(NOF)){
      if(i!=j){
        sde[i,j] <-  sqrt(sum((NOF[i,] - S.NOF[j,])^2))
      }
    }
  }
  
  return(sde)
}


# END ShiftedDistanceNGB
################################################################################




################################################################################
##                            DistanceFromIdeal                             ##
################################################################################
# Author : Rodrigo Marinao Rivas                                              ##
################################################################################
# Created: 2020                                                               ##
# Updates:                                                                    ##
#                                                                             ##
################################################################################
# Description: 'The porpuse of this function is to calculate the distance of
#               each particle of the matrix of normalised particle's position,
#               respect to the ideal point z* = (0, 0, ..., 0)
#
#
# Arguments:
# NOF: 'matrix' (even with one single column). Contains the normalised position 
#               of the particles in the OF hyperspace, i.e., the hyperspace of
#               the objective functions

# Value:
# 'numeric' - distance of each particle from ideal point
#
# Note: strictly speaking, it should be (NOF-zeros)^2, but NOF^2 is enough
#

DistanceFromIdeal <- function(NOF){
  
  dis <- sqrt(apply(NOF^2, FUN = sum, MARGIN = 1))
  
  return(dis)
}

# END DistanceFromIdeal
################################################################################



################################################################################
##                         CosineDistanceFromOnes                          ##
################################################################################
# Author : Rodrigo Marinao Rivas                                              ##
################################################################################
# Created: 2020                                                               ##
# Updates:                                                                    ##
#                                                                             ##
################################################################################
# Description: 'The purpose of this function is to calculate the cosine distance
#               of the normalised particle's position, from the point 
#               z* = (1, 1, ..., 1). A result of ones means closer to the
#               point z* = (1, 1, ..., 1)'
#
#
# Arguments:
# NOF: 'matrix' (even with one single column). Contains the normalised position 
#               of the particles in the OF hyperspace, i.e., the hyperspace of
#               the objective functions
# Value:
# 'numeric' - cosine distance of each normalised particle's position from 'ones'

CosineDistanceFromOnes <- function(NOF){
  a <- NOF # normalised particle's position
  b <- matrix(1, ncol = ncol(a), nrow = nrow(a)) # ones
  cosine.distance <- 1-apply(a*b, FUN = sum, MARGIN = 1)/(sqrt(apply(a^2, FUN = sum, MARGIN = 1))*sqrt(apply(b^2, FUN = sum, MARGIN = 1)))

  cosine.distance[cosine.distance<0] <- 0

  return(cosine.distance)
}
# END CosineDistanceFromOnes
################################################################################



################################################################################
##                                     BFE                                    ##
################################################################################
# Author : Rodrigo Marinao Rivas                                              ##
################################################################################
# Created: 2020                                                               ##
# Updates:                                                                    ##
#                                                                             ##
################################################################################
# Description: 'The purpose of this function is to calculate the 
#               balanceable fitness estimation for each particle'
#
#
# Arguments:
# sde: 'matrix' - Shifted Euclidian distance to the nearest neighbour, for the
#                 normalised particle position matrix
# Cv:  'numeric' - convergence distance
# d1:  'numeric' - projection distance of the particle position normalised from
#                  line joining the nadir (ones) to the ideal point (zeros)
# d2:  'numeric' - perpendicular distance of normalised particle's position from
#       line connecting nadir (ones) from ideal point (zeros)
#
# Value:
# 'numeric' - balanceable fitness estimation for each particle

BFE <- function(sde,Cv,d1,d2){
  alpha <- rep(0, length(Cv))
  beta <- rep(0, length(Cv))
  
  SDE <- apply(sde, MARGIN = 1, FUN = min)
  Cd  <- (SDE-min(SDE))/(max(SDE)-min(SDE))
  
  Cd_avg  <- mean(Cd)
  Cv_avg  <- mean(Cv)
  d1_avg  <- mean(d1)
  d2_avg  <- mean(d2)
  

  case111 <- Cv >  Cv_avg & d1 <= d1_avg & Cd <= Cd_avg
  case112 <- Cv >  Cv_avg & d1 <= d1_avg & Cd >  Cd_avg
  case121 <- Cv >  Cv_avg & d1 >  d1_avg & Cd <= Cd_avg
  case122 <- Cv >  Cv_avg & d1 >  d1_avg & Cd >  Cd_avg
  case211 <- Cv <= Cv_avg & d1 <= d1_avg & d2 >  d2_avg & Cd <= Cd_avg
  case212 <- Cv <= Cv_avg & d1 <= d1_avg & d2 >  d2_avg & Cd >  Cd_avg
  case221 <- Cv <= Cv_avg &(d1 >  d1_avg | d2 <= d2_avg)& Cd <= Cd_avg
  case222 <- Cv <= Cv_avg &(d1 >  d1_avg | d2 <= d2_avg)& Cd >  Cd_avg
  
  
  alpha[case111] <- runif(sum(case111), min = 0, max = 1)*0.3+0.8
  alpha[case112] <- 1
  alpha[case121] <- 0.6
  alpha[case122] <- 0.9
  alpha[case211] <- runif(sum(case211), min = 0, max = 1)*0.3+0.8
  alpha[case212] <- 1
  alpha[case221] <- 0.2
  alpha[case222] <- 1
  
  beta[case111] <- 1
  beta[case112] <- 1
  beta[case121] <- 1
  beta[case122] <- 1
  beta[case211] <- runif(sum(case211), min = 0, max = 1)*0.3+0.8
  beta[case212] <- 1
  beta[case221] <- 0.2
  beta[case222] <- 0.2
  
  BFE <- alpha*Cd #+ beta*Cv
  return(BFE)
}
# END BFE


################################################################################
##                              GeneticOperators                             ##
################################################################################
# Author : Rodrigo Marinao Rivas                                              ##
################################################################################
# Created: 2020                                                               ##
# Updates:                                                                    ##
#                                                                             ##
################################################################################
# Description: 'Application of genetic operators (Simulated binary crossover
#               followed by polynominal mutation) to obtain offsprings that 
#               give diversity to the exploration'
#
# Arguments:
# Param : 'matrix' - Population parameters that will be used as parents for the
#          application of genetic operators
# lower : 'numeric' - minimum possible value for each parameter
# upper : 'numeric' - maximum possible value for each parameter
#

GeneticOperators <- function(Param, lower = lower, upper = upper){
  Ancestry1 <- Param
  Ancestry2 <- Param[sample.int(ceiling(nrow(Param)/2), size = nrow(Param), replace = TRUE),]
  
  prob.cross <- 1  #is the probabilities of doing crossover
  dist.index <- 20 #distribution index of simulated binary crossover
  bits.mut <- 1  #expectation of number of bits doing mutation
  dist.pm <- 20 #distribution index of polynomial mutation
  
  N <- nrow(Ancestry1)
  D <- ncol(Ancestry1)
  
  beta <- matrix(0, ncol = D, nrow = N)
  mu <- matrix(runif(N*D, min = 0, max = 1), ncol = D, nrow = N)
  beta[mu<=0.5] <- (2*mu[mu<=0.5])^(1/(dist.index+1))
  beta[mu>0.5] <- (2-2*mu[mu>0.5])^(-1/(dist.index+1))
  beta <- beta*(-1)^matrix(sample(c(0,1), size = N*D, replace = TRUE), ncol = D, nrow = N)
  beta[matrix(runif(N*D, min = 0, max = 1), ncol = D, nrow = N)<0.5] <- 1
  beta[matrix(runif(N, min = 0, max = 1) > prob.cross, ncol = D, nrow = N, byrow = FALSE)] <- 1
  Offspring <- (Ancestry1+Ancestry2)/2+beta*(Ancestry1-Ancestry2)/2
  
  Lower <- matrix(rep(lower,N), nrow = N, ncol = D, byrow = TRUE)
  Upper <- matrix(rep(upper,N), nrow = N, ncol = D, byrow = TRUE)
  Site  <- matrix(runif(N*D, min = 0, max = 1), ncol = D, nrow = N) < bits.mut/D
  
  mu <- matrix(runif(N*D, min = 0, max = 1), ncol = D, nrow = N)
  temp  <- Site & mu<=0.5
  Offspring <- pmin(pmax(Offspring,Lower),Upper)#
  
  Offspring[temp] = Offspring[temp]+(Upper[temp]-Lower[temp])*((2*mu[temp]+(1-2*mu[temp])*
                                                                  (1-(Offspring[temp]-Lower[temp])/(Upper[temp]-Lower[temp]))^(dist.pm+1))^(1/(dist.pm+1))-1)
  
  temp = Site & mu>0.5
  Offspring[temp] = Offspring[temp]+(Upper[temp]-Lower[temp])*(1-(2*(1-mu[temp])+2*(mu[temp]-0.5)*
                                                                    (1-(Upper[temp]-Offspring[temp])/(Upper[temp]-Lower[temp]))^(dist.pm+1))^(1/(dist.pm+1)))
  
  # if(any(is.na(Offspring))){
  #   Offspring[is.na(Offspring)] <- Ancestry1[is.na(Offspring)]
  # }
  # 
  return(Offspring)
}
#END GeneticOperators  


TournamentRedux <- function(ChooseTou, CombineTou, SiTou){


  markTouOut <- vector(mode = "logical", length = length(ChooseTou) + 1)

  for(j in 1:length(ChooseTou)){

    flag <- any(SiTou < CombineTou[ChooseTou[j],]) - any(SiTou > CombineTou[ChooseTou[j],])
    
    if(all(SiTou == CombineTou[ChooseTou[j],])){
      flag <- -1
    }
    
    if(flag == 1){
      markTouOut[j] <- TRUE
    }else if(flag == -1){
      markTouOut[length(markTouOut)] <- TRUE
      break
    }

  }

  return(markTouOut)

}

################################################################################
##                          BFEandDominanceTwoSets                            ##
################################################################################
# Author : Rodrigo Marinao Rivas                                              ##
################################################################################
# Created: 2020                                                               ##
# Updates:                                                                    ##
#                                                                             ##
################################################################################

# Description: 'Calculate the BFE values of each particle and to decide which 
#               particles are kept in the repository. Dominance is evaluated 
#               between two sets: the Repository (A) and the new Population (S).'

# Arguments:
# A:              'matrix' - Current repository of particles
# S:              'matrix' - New population according to the respective iteration
# K:              'integer' - Maximum repository size

# Value:
# 'list'  -List with towo elements
#          Selection: 'logical' - Vector indicating whether or not the respective
#                                 particle is dominant (TRUE) or non-dominant
#          BFE:       'numeric' - BFE value for the respective particle


BFEandDominanceTwoSets <- function(A,S,K){
  
  Combine <- rbind(A,S) # merge of the current repository of particles plus new population (merged pre-repository)
  
  PopObj <- NormalizeObjFun(All = Combine, Rep = A) # normalization of the merged pre-repository
  sde <- ShiftedDistanceNGB(NOF = PopObj) # calculation of shifted distance neighborhood
  dis <- DistanceFromIdeal(NOF = PopObj) # distance from zenit point (ideal, i.e. point (0,0,0,0...) in these cases)
  Cv  <- 1 - dis
  cosine <- 1 - CosineDistanceFromOnes(NOF = PopObj) #cosine distance to nadir point (worst admissible, i.e. point (1,1,1,1,1...) in these cases)
  d1     <- dis*cosine
  d2     <- dis*sqrt(1-cosine^2)
  Choose <- c(1:nrow(A))
  
  for(i in 1:nrow(S)){
    
    #mark_0 <- vector(mode = "logical", length = length(Choose) + 1)

    mark <- TournamentRedux(ChooseTou = Choose, CombineTou = as.matrix(Combine), SiTou = as.numeric(S[i,]))
    
    Choose <- Choose[!mark[-length(mark)]]
    
    if(!mark[length(mark)]){
      Choose <- c(Choose, nrow(A)+i)
      
      # ISSUE PROCEDURE - procedure for each time the repository reaches its maximum capacity-----------------------
      
      if(length(Choose)>K){
        
        ranking <- rank(BFE(sde = sde[Choose, Choose], Cv = Cv[Choose], d1 = d1[Choose], d2 = d2[Choose]), ties.method = "random") # 1 is the lowest

        worst <- which(ranking == 1)
        
        Choose <- Choose[-worst]
      }
      
      # END ISSUE PROCEDURE---------------------------------
    }
  }
  
  is.dominant <- rep(FALSE, nrow(Combine))
  is.dominant[Choose] <- TRUE
  
  if(sum(is.dominant)>1){
    
    PopObj <- NormalizeObjFun(All = Combine[Choose,,drop=FALSE], Rep = NULL) # normalization of the merged pre-repository
    sde <- ShiftedDistanceNGB(NOF = PopObj) # calculation of shifted distance neighborhood
    dis <- DistanceFromIdeal(NOF = PopObj) # distance from zenit point (ideal, i.e. point (0,0,0,0...) in these cases)
    Cv  <- 1 - dis
    cosine <- 1 - CosineDistanceFromOnes(NOF = PopObj) #cosine distance to nadir point (worst admissible, i.e. point (1,1,1,1,1...) in these cases)
    d1     <- dis*cosine
    d2     <- dis*sqrt(1-cosine^2)

    bfe <- BFE(sde, Cv, d1, d2)
    bfe.all <- rep(NA, nrow(Combine))
    
    bfe.all[Choose] <- bfe
    
  }else{
    bfe.all <- rep(NA, length(is.dominant))
    bfe.all[is.dominant] <- 0
  }
  
  
  
  
  out.bfe <- list(Selection = is.dominant, BFE = bfe.all)
  
  return(out.bfe)
}
#END BFEandDominanceTwoSets




################################################################################
##                            BFEandDominanceOneSet                           ##
################################################################################
# Author : Rodrigo Marinao Rivas                                              ##
################################################################################
# Created: 2020                                                               ##
# Updates:                                                                    ##
#                                                                             ##
################################################################################

# Description: 'The purpose of this function is to calculate the BFE values of 
#               each particle and to decide which particles are kept in the 
#               repository. This function is applicable for a single set, of
#               which a priori the dominant particle(s) are unknown.'

# Arguments:
# A:              'matrix' - Current repository of particles
# K:              'integer' - Maximum repository size

# Value:
# 'list'  -List with towo elements
#          Selection: 'logical' - Vector indicating whether or not the respective
#                                 particle is dominant (TRUE) or non-dominant
#          BFE:       'numeric' - BFE value for the respective particle

BFEandDominanceOneSet <- function(A,K = NULL){
  
  size.now <- nrow(A)
  
  is.dominant <- rep(TRUE, size.now)
  

  for (i in 1:size.now ){
    contender <- A[i,]
    for(j in 1:size.now){
      if(i!=j){
        opponent <- A[j,]
        if(all(contender > opponent)){
          is.dominant[i] <- FALSE
          break
        }
      }
    }
  }
  
  if(sum(is.dominant)>1){
    PopObj <- NormalizeObjFun(All = A[is.dominant,,drop = FALSE], Rep = NULL)
    sde <- ShiftedDistanceNGB(NOF = PopObj)
    dis <- DistanceFromIdeal(NOF = PopObj)
    Cv  <- 1 - dis
    cosine <- 1 - CosineDistanceFromOnes(NOF = PopObj)

    d1     <- dis*cosine
    d2     <- dis*sqrt(1-cosine^2)

    
    bfe <- BFE(sde, Cv, d1, d2)
    
    if(!is.null(K) & is.numeric(K) & K>1){
      
      if(sum(is.dominant)>K){
        
        K <- ceiling(K)
        
        rank.bfe <- rank(-bfe, ties.method  = "random")
        top.max <- rank.bfe %in% seq(1,K)
        
        is.dominant[is.dominant] <- top.max
        
        PopObj <- NormalizeObjFun(All = A[is.dominant,,drop = FALSE], Rep = NULL)
        sde <- ShiftedDistanceNGB(NOF = PopObj)
        dis <- DistanceFromIdeal(NOF = PopObj)
        Cv  <- 1 - dis
        cosine <- 1 - CosineDistanceFromOnes(NOF = PopObj)
        d1     <- dis*cosine
        d2     <- dis*sqrt(1-cosine^2)
        

        bfe <- BFE(sde, Cv, d1, d2)

      }
    }else{
      stop('The argument K must be a numeric (if possible, an integer) greater than zero')
    }
    
    
    bfe.all <- rep(NA, length(is.dominant))
    bfe.all[is.dominant] <- bfe
  }else{
    bfe.all <- rep(NA, length(is.dominant))
    bfe.all[is.dominant] <- 0
  }
  
  
  out.bfe <- list(Selection = is.dominant, BFE = bfe.all)
  
  return(out.bfe)
}
