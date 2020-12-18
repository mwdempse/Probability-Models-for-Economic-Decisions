
"Authors: Matthew Dempsey & Harrison McKenny"

library(tidyverse)


# UTIL Function
UTIL <- function(x,r,s){
  #######################
  # x: any amount of monetary income
  # r: risk tolerance constant
  # s: risk tolerance slope
  # output: expected utility or money amount given a risk tolerance
  #######################
  
  if(missing(s) & r>0){
    EUtility <- -exp(-x/r)
    return(EUtility)
  }
  else if (missing(s) & r<0){
    EUtility <- exp(-x/r)
    return(EUtility)
  }
  else {
    
    utl <- log(r+s*x)
    EUtility <- exp(utl*(1-1/s))/(s-1)
    return(EUtility)
  }
  
}

UTIL(2,2)

# Risk Tolerance Function
RISKTOL <- function(H,L,CE){
  #######################
  # H: High monetary possibility
  # L: Low monetary possibility 
  # CE: Certainty Equivalent 
  # output: Equivalent Risk Tolerance
  #######################
  
  if(H<=L){
    stop("Error H cannot be less than L")
  } else{
    rat=(CE-L)/(H-L)
    k=1
    if(rat==0.5){
      stop("Error")
    } else{
      if (rat>0.5){
        rat=1-rat
        k=-1
      } 
      if (rat<0){
        stop("Error")
      } else{
        x=0.1251/(0.5-rat)
        z=rat/0.6932
        
        for (i in 1:15){
          y=(x+z)/2
          if (-y*log(0.5*exp(-1/y)+0.5)>rat){
            x=y 
          }else{
            z=y
          }
          
        }
        a=-x*log(0.5*exp(-1/x)+0.5)
        b=-z*log(0.5*exp(-1/z)+0.5)
        if (a!=b){
          y=z+(x-z)*(rat-b)/(a-b)
          RT=k*(H-L)*y
          return(RT)
        } 
        else{
          RT=k*(H-L)*(x+z)/2 
          return(RT)
        }
      }
    }
  }
}

RISKTOL(20,-10,2)

UINV <- function(u,r,s){
  #######################
  # u: any amount of monetary income
  # r: risk tolerance constant
  # s: risk tolerance slope
  # output: expected money which has an equivalent value to the provided utility given a risk tolerance
  #######################
  
  if(missing(s) & r > 0 ){
    ans <-  -r*log(-u)
    return(ans)
  }
  else if (missing(s) & r < 0 ){
    utl <- -u
    ans <-  -r*log(-utl)
    return(ans)
  }
  else if (s!=0 & s!=1){
    utl <- log((s-1)*u)/(1-1/s)
    ans <- (exp(utl)-r)/s
    return(ans)
  }
  else if(s==1){
    ans <- (exp(u)-r)/s
    return(ans)
  }
  else{
    print("Error")
  }
}

# Certainty Equivalent if utility is endogenous
CEPR <- function(prize,proby,r,s){
  
  Util_Prize_Rt <- UTIL(prize,r,s)
  EU <- as.vector(crossprod(Util_Prize_Rt,proby))
  CE <- UINV(EU,r,s)
  return(CE)
}

# Certainty Equivalent if utility is endogenous
CE <- function(EMV,R,s){
  b15 <- UTIL(EMV,R,s)
  avgr <- mean(b15)
  equiv<- UINV(avgr,R,s)
  return(equiv)
}


# Other commonly used functions

STDEVPR <- function(x,prob){
  #######################
  # x: vector of values
  # prob: vector of probabilities
  # output: returns standard deviation of profit
  #######################
  
  meank<- as.vector(crossprod(x,prob))
  d <- (x-c(meank))^2
  stdevk <- as.vector(crossprod(d,prob)^0.5)
  
  return(stdevk)
}


# A simpler way to get the same results as the DISCRINV function  is to use the sample function in base R
DISCRINV <- function(x,values,prob){
  #######################
  # x: randomly generated value between 0 and 1
  # values: vector of values
  # prob: vector of probabilities
  # output: returns standard deviation of profit
  #######################
  
  require(FSA)
  
  if(length(values) <3 ){
    ifelse(x<0.5,return(values[1]),return(values[2]))
  }
  
  precumsum <- c(pcumsum(prob),1)
  
  for (i in 1:(length(precumsum)-1)){
    if (precumsum[i] <= x & x < precumsum[i+1]){
      simulatedvalue <- K[i]
    }
  }
  
  return(simulatedvalue)
  
}

# Standard Normal distribution with mean 0 and Standard Deviation 1
NORM.S.DIST <- function(p,cp = T){
  #######################
  # p: value to evaluate
  # cp: TRUE: Densitiy; FALSE: Cumulative Probability   
  # output: returns standard deviation of profit
  #######################
  if(!is.logical(cp)){
    stop('cp must be boolean value')
  }
  if(cp){
    ans <- dnorm(p)
    return(ans)
  }else{
    ans <- pnorm(p)
    return(ans)
  }
}


# 
GENLINV <- function(prob1,quart1,quart2,quart3,lowest=NULL,highest=NULL){
  
  prob2 = ifelse(prob1>0.999999,0.999999,
                 ifelse(prob1<0.000001,0.000001,prob1))
  
  norml = qnorm(prob2)/0.67449
  
  b = (quart3 - quart2)/(quart2-quart1)
  
  if(quart1>quart3 | quart1>quart2){
    stop("Number Error quart1 > quart2|quart3")
  }
  
  if(b==1){
    GenAns = (quart3-quart2)*norml+quart2
  }
  else if(b>0){
    GenAns = (quart3-quart2)*(b^norml-1)/(b-1)+quart2
  }
  else{
    stop("Number Error")
  }
  if(!is.null(lowest)){
    if(quart1<lowest|quart2<lowest|quart3<lowest){
      stop("Number Error quart < lowest")
    }
    else if(GenAns < lowest){
      GenAns = lowest
    }
  }
  if(!is.null(highest)){
    if(quart3>highest | quart1>highest | quart2>highest){
      stop("Number Error quart > highest ")
    }
    else if(GenAns > highest){
      GenAns = highest
    }
  }
  return(GenAns)
}

# Certainty Equivalent given a normal distribution

CE_Norm <- function(mu, std, crt) {
  if (missing(std)|missing(crt)|missing(mu)) {
    stop('Mean, Standard Deviation, or Risk Tolerance are missing')
  }
  ce = mu - 0.5*(std^2)/crt
}

# Risk Premium given a normal distribution

RP_Norm <- function(std, crt, ce = NULL,mu = NULL) {
  if (missing(std)|missing(crt)) {
    stop('Standard Deviation and Risk Tolerance are required')
  }
  
  if (is.null(ce)) {
    rp = .05*(std^2)/crt
    return(rp)
  } 
  
  if (!is.null(mu)) {
    rp = mu - ce
    return(rp)
  } else{
    stop('Mean is required')
  }
}


# qlnorm function which takes nonlog mean and standard deviations

LNORMINV <- function(p, mean, sd) {
  sd2 = log(1 + ((sd / mean) ^ 2))
  lnorm = exp(qnorm(p,mean  = log(mean) - (0.5 * sd2) , sd = sd2^0.5))
  return(lnorm)
}

# dlnorm function which takes nonlog mean and standard deviations

LNORMINV.D <- function(x, mean, sd) {
  sd2 = log(1 + ((sd / mean) ^ 2))
  dlnorm = dlnorm(x,mean  = log(mean) - (0.5 * sd2) , sd = sd2^0.5)
  return(dlnorm)
}


# qgamma which takes mean and standard deviations

GAMINV <- function(p, mean, sd) {
  gamma = qgamma(p, shape = (mean / sd) ^ 2, rate =  mean/(sd ^ 2))
return(gamma)
}

# dgamma which takes mean and standard deviations

GAMINV.D <- function(x, mean, sd) {
  dgamma = dgamma(x, shape = (mean / sd) ^ 2, rate = mean/(sd ^ 2))
return(dgamma)
}

# qbeta which takes mean and standard deviations

BETINV <- function(p, mean, sd, lowerbound = NULL,  upperbound = NULL) {
  u = 1
  L = 0
  
  if (!is.null(lowerbound)) L = lowerbound
  if (!is.null(upperbound)) u = upperbound
  if (u <= L) stop('Upperbound smaller than Lowerbound')
  
  m = (mean - L) / (u - L)
  s = sd / (u - L)
  n = m * (1 - m) / (s ^ 2) - 1
  
  beta = L + (u - L) * qbeta(p, shape1 =  m * n, shape2 = (1 - m) * n)
  
  return(beta)
}

# dbeta which takes mean and standard deviations

BETINV.D <- function(x, mean, sd, lowerbound = NULL,  upperbound = NULL) {
  u = 1
  L = 0
  
  if (!is.null(lowerbound)) L = lowerbound
  if (!is.null(upperbound)) u = upperbound
  if (u <= L) stop('Upperbound smaller than Lowerbound')
  
  m = (mean - L) / (u - L)
  s = sd / (u - L)
  n = m * (1 - m) / (s ^ 2) - 1
  
  beta = L + (u - L) * dbeta(x, shape1 =  m * n, shape2 = (1 - m) * n)
  
  return(beta)
}

# Creates a matrix of products

PRODS = function(x){
  n = length(x)
  result = matrix(nrow = n,ncol = n)
    
  for (i in 1:n) {
    for (k in 1:n) {
      result[i,k] = x[i] * x[k]
    }
  }
    
  colnames(result) = x
  rownames(result) = x
  return(result)
}

# Covariance between two vectors given a probability of outcomes

COVARPR <- function(val1, val2, prob) {
  
  c = NCOL(prob)
  r = length(prob)
  
  if (length(val1) != r || length(val2) != r) {stop('Length of vectors not equal')}
  if (NCOL(val1) != c | NCOL(val2) != c) stop('Number of columns in vectors not equal')
  if (sum(prob) != 1) stop('Prob does not add up to one')
  
  sump = 0
  mean1 = 0
  mean2 = 0
  prods = 0
  
  if (c == 1) {
    for (i in 1:r) {
      
      p = prob[i]
      sump = sump + p
      mean1 = mean1 + val1[i] * p
      mean2 = mean2 + val2[i] * p
      prods = prods + val1[i] * val2[i] * p
      
    }
  }else{
    for (i in 1:r) {
      for (k in 1:c) {
        p = prob[i,k]
        sump = sump + p
        mean1 = mean1 + val1[i, k] * p
        mean2 = mean2 + val2[i, k] * p
        prods = prods + val1[i, k] * val2[i, k] * p
      }
    }
  }
  
  
  results = (prods - (mean1 * mean2 / sump)) / sump
  return(results)
  
}

# Pearson correlation between two vectors given a probability of outcomes

CORRELPR <- function(val1, val2, prob){
  
  results = COVARPR(val1, val2, prob) / ((COVARPR(val1, val1, prob) * COVARPR(val2, val2, prob)) ^ 0.5) 
  return(results)
  
}
