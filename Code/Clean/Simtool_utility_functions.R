#----Function Creations---------
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
  EU <- crossprod(Util_Prize_Rt,proby)
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


# Other commonly used functions.

STDEVPR <- function(x,prob){
  #######################
  # x: vector of values
  # prob: vector of probabilities
  # output: returns standard deviation of profit
  #######################
  
  meank<- crossprod(x,prob)
  d <- (x-c(meank))^2
  stdevk <- (crossprod(d,prob)^0.5)
  
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

