rm(list = ls())

"Authors: Matthew Dempsey & Harrison McKenny"

# Packages
library(tidyverse)

# Load in Simtool specific functions from GITHUB
source('https://raw.githubusercontent.com/mwdempse/Probability-Models-for-Economic-Decisions/main/Code/Clean/Simtool_utility_functions.R')

# Values from CH2
K <- seq(1,5,1)
prob <- c(0.1,0.25,0.3,0.25,0.1)
totmkt <- 100
fc <- 26
profit_mat <- cbind.data.frame(K,prob)
names(profit_mat) <- c('K','P(K=k)')
profit <- unname(unlist(profit_mat %>% transmute(Profit = totmkt/(1+K) - fc)))


# Fig 3.1 Utility function with constant risk tolerance for Superconductor example from CH2

plot.cr <- function(H,L,CE) {
  #######################
  # H: High monetary possibility
  # L: Low monetary possibility 
  # CE: Certainty Equivalent 
  # output: List containing the risk tolerance, Expected Utility, And plot of 
  #######################
  
  R <- RISKTOL(H,L,CE)
  EU <- .5*UTIL(H,R) + 0.5*UTIL(L,R)
  `U(CE)` <- UTIL(CE,R)
  X <- seq(L-10,H+50,5)
  U <- UTIL(X,R)
  inv <- UINV(U,R)
  
  Expected_Utility <- data.frame(EU,`U(CE)`)
  table <- data.frame(X,U,inv)
  names(table) <- c("$X","UTIL(X,RT)","UINV(U,RT)")
  
  xlab_frame <- data.frame(xlab = X)
  ylab_frame <- data.frame(ylab = seq(plyr::round_any(min(table$`UTIL(X,RT)`),1,floor),
                                      plyr::round_any(max(table$`UTIL(X,RT)`),1,ceiling),.2))
  
  g <- ggplot(table, aes(y=`UTIL(X,RT)`,x=X)) +
    labs(y=paste0("Utility, with RiskTol=",round(R,2)), x = "Monetary Income") +
    geom_line(size = 1.2)+
    # y axis line
    geom_segment(x = 0, xend = 0, 
                 y = plyr::round_any(min(table$`UTIL(X,RT)`),1,floor)-1, yend = plyr::round_any(max(table$`UTIL(X,RT)`),1,ceiling),
                 size = 0.5) +
    # x axis line
    geom_segment(y = 0, yend = 0, 
                 x = min(X)-5, xend = max(X)+5,
                 size = 0.5) +
    # x ticks
    geom_segment(aes(y = 0, yend = 0, 
                     x = min(X), xend = max(X))) +
    # y ticks
    geom_segment(aes(y = plyr::round_any(min(`UTIL(X,RT)`),1,floor), yend = plyr::round_any(max(`UTIL(X,RT)`),1,ceiling), 
                     x = 0, xend = 0)) + 
    # labels
    geom_text(data=xlab_frame, aes(x=xlab, y=0, label=xlab),
              vjust=1.5) +
    geom_text(data=ylab_frame, aes(x=0, y=ylab, label=ylab),
              hjust=1.5) +
    scale_x_discrete(position = "top") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank())
  
  
  results<- list(R,Expected_Utility,table,g)
  names(results) <- c('Risk_Tolerance', 'Expected_Utility','Table', "Graph")
  return(results)

}

plot.cr(20,-10,2)

# Fig 3.2 Analysis of Superconductor example from CH2 with constant risk tolerance

utility_sim <- function(H,L,CE, profit, prob, n = 400) {
  #######################
  # H: High monetary possibility
  # L: Low monetary possibility 
  # CE: Certainty Equivalent 
  # profit: vector of expected profits
  # prob: vector of probabilities related to expected profits
  # n: number of simulations
  # output: List containing the risk tolerance, Expected Utility, And plot of 
  #######################
  
  R <- RISKTOL(H,L,CE)
  `U(Profit,RT)` <- UTIL(profit,R)
  EU <- as.vector(crossprod(`U(Profit,RT)`,prob))
  EMV <- as.vector(crossprod(profit,prob))
  ce <- CEPR(profit,prob,R)
  
  utility_table <- cbind.data.frame(profit,prob,`U(Profit,RT)`)
  
  simtable <- as.data.frame(sample(profit,n,replace = T, prob = prob)) 
  names(simtable) <- 'profit'
  simtable <- simtable %>% left_join(utility_table) %>% select(-prob)
  sim_EMV <- mean(simtable$profit)
  sim_ce <- CE(simtable$profit,R)
  sim_Risk_Premium <- sim_EMV - sim_ce
  sim_EU <- mean(simtable$`U(Profit,RT)`)
  sim_utility_sd <- sd(simtable$`U(Profit,RT)`)
  utility_95_CI <- c(sim_EU-1.96*sim_utility_sd/n^0.5,sim_EU+1.96*sim_utility_sd/n^0.5)
  certanty_equivilant_95_CI <- UINV(utility_95_CI,R)
  
  utility_table_estimates <- list(EMV,ce,EU)
  names(utility_table_estimates) <- c('EMV', 'CE', 'EU')
  
  simulation_estimates <- list(sim_EMV,sim_ce,sim_Risk_Premium,sim_EU,sim_utility_sd,utility_95_CI)
  names(simulation_estimates) <- c('Simulated_EMV', 'Simulated_CE', 'Simulated_Risk_Premium',
                                   'Simulated_EU','Simulated_Utility_SD','Simulated_Utility_CI_95%')
  
  results<- list(R,utility_table_estimates,utility_table,simulation_estimates,simtable)
  names(results) <- c('Risk_Tolerance', 'Utility_Table_Estimates','Utility_Table', "Simulation_Estimates",'Simtable')
  return(results)
  
}

utility_sim(20,-10,2,profit = profit, prob = prob)

# Fig 3.3 Estimating local risk tolerances with constant or linear risk tolerance

RT_local <- function(X,R,s,d = 10) {
  #######################
  # X: Vector of Monetary Values
  # R: Risk Tolerance 
  # s: Risk Tolerance Slope 
  # d: Payout Difference
  # output: Dataframe of local risk tolerances
  #######################
  
  less_d = X - d
  with_d = X + d
  
  # Linear Risk Tolerance
  # constant risk premium
  crp <- X-CE(c(with_d,less_d),R)
  # constant local risk tolerance
  clrt <- 0.5*(d^2)/crp 
  
  # Linear Risk Tolerance
  # linear risk premium
  lrp <- X-CE(c(with_d,less_d),R,s)
  # linear local risk tolerance
  llrt <- 0.5*(d^2)/crp 
  
  
  results <- data.frame(X,with_d,less_d,crp,clrt,lrp,llrt)
  names(results) <- c("X","X+d","X-d",
                      'Constant Risk Premium', 'Local Constant Risk Tolerance',
                      'Linear Risk Premium', 'Local Linear Risk Tolerance')
  
  return(results)
  
}

RT_local(X = c(1000,5000,10000,20000,50000), R = 10000, s = .02, d = 10)

# Fig 3.4 Analysis of utility theory between two lotteries

lottery_utilities <- function(profit, prob, 
                   lot1_prob = c(.5,.5),
                   lot2_prob = c(.6,.4),
                   D3 = F){
  #######################
  # profit: vector of expected profits
  # prob: vector of probabilities related to expected profits
  # lot1_prob: Probabilities of Lottery 1
  # lot2_prob: Probabilities of Lottery 2
  # output: Dataframe of utility of possible lottery combinations with corresponding graph
  #######################
 
  profprob = cbind.data.frame(profit,prob)
  
  combo <- data.frame(t(combn(profit,2)))
  names(combo) <- c('Low','High')
  combo <- combo %>% 
    left_join(profprob, by = c('Low' = 'profit')) %>% 
    left_join(profprob, by = c('High' = 'profit'), suffix = c("_low","_high"))
  
  Utility1 = vector()
  Utility2 = vector()
  
  for (i in 1:length(combo[,1])) {
    
    Utility1[i] = as.vector(crossprod(lot1_prob,c(combo$prob_low[i], combo$prob_high[i])))
    Utility2[i] = as.vector(crossprod(lot2_prob,c(combo$prob_low[i], combo$prob_high[i])))
    
  }
  combo <- cbind(combo,Utility1,Utility2)
  
  dat = combo %>% select(-c(prob_low,prob_high,Utility2)) %>% rename(Utility = Utility1) %>% mutate(Lottery = 'First Lottery')
  dat2 = combo %>% select(-c(prob_low,prob_high,Utility1)) %>% rename(Utility = Utility2) %>% mutate(Lottery = 'Second Lottery')
  plotdat = rbind.data.frame(dat,dat2) %>%
    mutate(Lot = paste(Low,High,sep = ','),
           row = ifelse(row_number() > length(Lot)/2,row_number()-length(Lot)/2,row_number()))
  colors <- ifelse(plotdat$Lottery == 'First Lottery',"#56B4E9","#E69F00")

  g = ggplot(plotdat, aes(x = reorder(Lot,row), y = Utility, fill = Lottery)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values = c('gold2','blue3')) + 
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 60 ),
          panel.background = element_blank())
  
  results = list(combo,g)
  return(results)
}

lottery_utilities(profit = c(0,920,1000,2000,3300,5000),
       prob =  c(0,0.25,0.27,0.5,0.75,1))


# Fig 3.5 Derivation of exponential utility from constant risk tolerance					

utility_con_RT <- function(prob = 0.51 ,U0 = 0, U1 = 1, kmax = 20) {
  #######################
  # prob: probability of winning a lotery
  # U0: Arbitrary utility of 0 money won
  # U1: Arbitrary utility of 1 money won
  # output: Dataframe of utility of lottery with k winnings
  #######################
  
  k = seq(0,kmax,1)
  R = 1/log(prob/(1-prob))
  Util = -exp(-k/R)
  `U(k)` = c(U0,U1)
  for (i in 1:19){
    `U(k)`[i+2] <- (`U(k)`[i+1]-(1-prob)*`U(k)`[i])/prob
  }
  
  A = (U1-U0)/(Util[1]-Util[2])
  B = `U(k)`[1]-A*Util[1]
  `A*UTIL+B` = A*Util+B
  
  results = data.frame(k,`U(k)`,Util,`A*UTIL+B`)
  return(results)
}

utility_con_RT(prob = 0.51 ,U0 = 0, U1 = 1, kmax = 20)
