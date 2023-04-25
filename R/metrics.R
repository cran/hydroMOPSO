
HV <- function(data, nadir.point.PF, n.samples){
  
  
  nad <- rep(1, ncol(data))
  max <- matrix(rep(nadir.point.PF, nrow(data)), ncol = ncol(data), byrow = TRUE)
  min <- matrix(rep(pmin(apply(data, FUN = min, MARGIN = 2),rep(0, ncol(data))), nrow(data)),
                ncol = ncol(data), byrow = TRUE)
  data <- (data-min)/(1.1*(max-min)) # PopObj
  
  del <- apply(data>1, FUN = any, MARGIN = 1)
  
  #print(data)
  data <- data[!del,,drop = FALSE]
  #print(nrow(data))
  if(nrow(data)<1){
    grade <- 0
  }else{

    refpoint <- nad
    
    ########
    
    maxvalue <- refpoint
    minvalue <- apply(data, FUN = min, MARGIN = 2)
    
    lower <- matrix( rep(minvalue, n.samples), nrow=n.samples, byrow=TRUE)
    upper <- matrix( rep(maxvalue, n.samples), nrow=n.samples, byrow=TRUE)
    
    samples <- matrix(runif(ncol(data)*n.samples, min=0, max=1), nrow=n.samples, ncol=ncol(data))
    #samples <- randtoolbox::sobol(n=n.samples, dim=ncol(data), scrambling=3, seed = floor(runif(1, 1, 99999)))  
    
    samples  <- lower + (upper-lower)*samples
    
    
    ######
    
    for(j in 1:nrow(data)){
      domi <- rep(TRUE, nrow(samples))
      
      m <- 1
      
      while(m<=ncol(data) & any(domi)){
        domi <- domi & data[j,m] <= samples[,m]
        m <- m+1
      }
      samples <- samples[!domi,]
    }
    
    grade <- prod(maxvalue-minvalue)*(1-nrow(samples)/n.samples)
    
    
  }
  
  return(grade)

}

