rm(list = ls())

"Authors: Matthew Dempsey & Harrison McKenny"

library(tidyverse)

# Values from Semiconductor example of Myerson, Zambrano
# possible number of competitors
K <- seq(1,5,1)
# Probability of each number competitors entering market
prob <- c(0.1,0.25,0.3,0.25,0.1)

# plot probabilities and CDF
plot.prob.cdf <- function(x,prob,xlab = "Number Of Competitive Entrants (K)",
                          title = "Figure 2.1: Discrete Probability Distribution For The Number Of Entrants (K)") {
  #######################
  # x: vector of values
  # prob: vector of probabilities
  # xlab: x label, default is set to figure 2.1 of Myerson, Zambrano
  # title: Title of chart
  # output: chart of probabilities and cdf, default is set to figure 2.1 of Myerson, Zambrano
  #######################
  
  plot(x,prob,
       type="h",
       xlim=c(0,6),
       ylim=c(0,1),
       xlab=xlab,
       ylab="Probability",
       main=title,
       lwd=3,
       yaxt="n")
  
  ticks <- seq(0,1,0.1) #customizing y axis
  axis(2,at=ticks,labels=ticks) #appending y axis
  lines(stepfun(x,cumsum(c(0,prob))),lty=2)
  text(x=1.5,y=0.5,"Dashed lines: Cumulative probability",cex=0.6)
  
} 

plot.prob.cdf(x=K, prob = prob)


# plot CDF
plot.cdf <- function(x,prob,ylab="Cumulative Probability",
                     title="Figure 2.2: Inverse Cumulative Probability Distribution For The Number Of Entrants (K)",
                     add = F) {
  #######################
  # x: vector of values
  # prob: vector of probabilities
  # ylab: x label, default is set to figure 2.2 of Myerson, Zambrano
  # title: Title of chart
  # output: chart of cdf, default is set to figure 2.2 of Myerson, Zambrano
  #######################
  
  plot(stepfun(cumsum(prob[1:(length(prob)-1)]),c(x)),
       ylim=c(0,length(x)+1),
       xlim=c(0,1),
       ylab=ylab,
       xlab="Cumulative Probability",
       main=title,
       xaxt="n",
       add = add)
  
  #customizing xaxis
  ticks <- seq(0,1,0.1)
  axis(1,at=ticks,labels=ticks)
  
}

plot.cdf(K,prob = prob)



# Calculate Profit based on number of competitors assuming total market value of 100 and fixed cost of 26

totmkt <- 100
fc <- 26


profit_mat <- function(x,prob,totalmkt,fc) {
  #######################
  # x: vector of values
  # prob: vector of probabilities
  # totalmkt: total market value
  # fc: fixed cost
  # output: returns dataframe of profit
  #######################
  
  profit_mat <- cbind.data.frame(x,prob)
  names(profit_mat) <- c('K','P(K=k)')
  profit_mat <- profit_mat %>% mutate(Profit = totalmkt/(1+K) - fc)
  return(profit_mat)
  
}

(p_mat<- profit_mat(K,prob,totalmkt = totmkt,fc))


# Find expected value and standard deviation of profit from profit matrix
(ex_value <- crossprod(p_mat$`P(K=k)`,p_mat$Profit))



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

(STDEVPR(p_mat$Profit,p_mat$`P(K=k)`))

# test estimates to simulated data

# Fig 2.6/2.7
sim_competition <- function(profit_mat,n = 1000){
  #######################
  # profit_mat: profit matrix from profit_mat function
  # n: number of simulations
  # output: returns simulated mean and standard deviation of profit
  #######################
  
  sam <- sample(profit_mat$K, size = n,prob = profit_mat$`P(K=k)`, replace = T)
  
  sim_mean = mean(sam)
  squared_sd_from_mean = (sam-sim_mean)^2
  
  sim = cbind.data.frame(sam,squared_sd_from_mean)
  names(sim) <- c("Sim'd K", 'Squared deviations from sample mean')
  results = list(sim_mean,sd(sam),sim)
  names(results)<-c('Sample Mean', 'SD', 'Simulation')
  return(results)
}

comp <- sim_competition(p_mat, n=400)
comp[1:2]
comp$Simulation[1:10,]


# plot cdf of simulated value

g <- ggplot(comp[[3]],aes(`Sim'd K`)) +
  stat_ecdf(geom = 'step', pad = F) +
  labs(y='Cumulative probability', x = 'Number of competive entrants (K)') +
  coord_flip() + 
  ggtitle('Simulated CDF') +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g

# Fig 2.8
sample_properties <- function(x, prob, n=30, samp = F) {
  #######################
  # x: vector of values
  # prob: vector of probabilities
  # n: number of samples
  # output: returns simulated mean and standard deviation vs the real expected value and standard deviation
  #######################
  
  ex_val = crossprod(x,prob)
  sdv = STDEVPR(x,prob)
  sdv_of_ex_val = sdv/sqrt(n)
 
  sam <- sample(x, size = n,prob = prob, replace = T)
  sam_mean <- mean(sam)
  sam_sd <- sd(sam)
  sam_sd_of_mean = sam_sd/sqrt(n)
  
  conintLow = sam_mean-1.96*sam_sd_of_mean
  conintHi = sam_mean+1.96*sam_sd_of_mean
  
  ex_val_in_95 = ex_val > conintLow & ex_val < conintHi
  
  cum_prob_.05 = unname(quantile(sam,0.05))
  
  if(samp){
    results = list(data.frame(ex_val,sam_mean,
                         sdv,sam_sd,
                         sdv_of_ex_val, sam_sd_of_mean,
                         cum_prob_.05,ex_val_in_95),
                   sam,
                   x)
    
  }else{
    results = data.frame(ex_val,sam_mean,
                         sdv,sam_sd,
                         sdv_of_ex_val, sam_sd_of_mean,
                         cum_prob_.05,ex_val_in_95)
    
  }
  
  return(results)
}

(sample_properties(x = K,prob = prob, n = 30))

# At 95% confidence we would exect the true expected value to be observed in the confidence interval of the simulated values ~5%
test = data.frame()
for (i in 1:1000) {
  test <- rbind.data.frame(test,sample_properties(x = K,prob = prob, n = 500))
}
1-sum(test$ex_val_in_95)/1000

# Fig 2.9 same as Fig 2.8 only with profit instead of nuymber of competitors

(sample_properties(x = p_mat$Profit,prob = p_mat$`P(K=k)`, n = 500))


# Fig 2.10
#create data
profit_sim <- sample_properties(x = p_mat$Profit,prob = p_mat$`P(K=k)`, n = 500, samp = T)
profits <- data.frame(profit_sim[[2]])
names(profits) <- 'exp_profit'

#create ticks for both x and y axes
xlab_frame <- data.frame(xlab = seq(0, 1,.1))
ylab_frame <- data.frame(ylab = seq(-10,25,5))

g2 <- ggplot(profits, aes(exp_profit)) +
        stat_ecdf(geom = 'step', pad = F) +
        labs(y='Cumulative probability', x = 'Profit  ($millions)') +
        coord_flip() + 
        # y axis line
        geom_segment(x = 0, xend = 0, 
                     y = min(profits), yend = max(profits),
                     size = 0.5) +
        # x axis line
        geom_segment(y = 0, yend = 0, 
                     x = 0, xend = 1,
                     size = 0.5) +
        # y axis to cumulative function line
        geom_segment(y = 0, yend = ecdf(profits$exp_profit)(min(profits)), 
                     x = min(profits), xend = min(profits),
                     size = 0.5) +
        
        # x ticks
        geom_segment(aes(y = 0, yend = 1, 
                         x = 0, xend = 0 + .0000001)) +
        # y ticks
        geom_segment(aes(y = 0, yend = 0 + .0000001, 
                         x = -10, xend = 25)) + 
        
        # labels
        geom_text(data=xlab_frame, aes(x=0, y=xlab, label=xlab),
                  vjust=1.5) +
        geom_text(data=ylab_frame, aes(x=ylab, y=0, label=ylab),
                  hjust=1.5) +
        ggtitle('CDF of Simulated Profits') +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_blank())
g2

# Fig 2.11 Simulation of market if competitors, development cost and market value are unknown

market_sim <- function(comp,comp_prob,
                       dev_cost, dev_prob,
                       mkt_val,mkt_prob,
                       n = 500) {
  #######################
  # comp: vector of possible number of competitors
  # comp_prob: vector of probabilities of possible number of competitors
  # dev_cost: vector of possible development costs
  # dev_prob: vector of probabilities of possible development costs
  # mkt_val: vector of possible of market values
  # mkt_prob: vector of probabilities of possible market values
  # n: number of samples
  # output: returns list of dataframes of simulation and corresponding summary statistics
  #######################
  
  com = sample(comp, prob = comp_prob, size = n, replace = T)
  dev = sample(dev_cost, prob = dev_prob, size = n,replace = T)
  mkt = sample(mkt_val, prob = mkt_prob, size = n,replace = T)
  
  sim = cbind.data.frame(com,dev,mkt)
  names(sim) <- c('Competitors','Development','Total_Market')
  sim <- sim %>% mutate(Profit = Total_Market/(1+Competitors) - Development) 
  
  mean_profit <- mean(sim$Profit)
  sd_profit <- sd(sim$Profit)
  sim_sd_of_mean = sd_profit/sqrt(n)
  
  conintLow = mean_profit-1.96*sim_sd_of_mean
  conintHi = mean_profit+1.96*sim_sd_of_mean
  
  cum_prob_.05 = unname(quantile(sim$Profit,0.05))
  
  results <- list(sim,data.frame(mean_profit,sd_profit,sim_sd_of_mean,conintLow,conintHi,cum_prob_.05))
  
  return(results)
  
}

# Fig 2.11 Market
comp <- seq(1,5,1)
comp_prob <- c(0.1,0.25,0.3,0.25,0.1)
dev_cost <- c(20,26,30,34)
dev_prob <- c(0.2,0.5,0.2,0.1)
mkt_val <- c(70,100,120,150)
mkt_prob <- c(0.3,0.4,0.2,0.1)

mkt_sim <- market_sim(comp = comp,comp_prob = comp_prob,
           dev_cost = dev_cost, dev_prob = dev_prob,
           mkt_val = mkt_val,mkt_prob = mkt_prob,
           n = 500)

# Fig 2.11 Chart

#create ticks for both x and y axes
xlab_frame <- data.frame(xlab = seq(0, 1,.1))
ylab_frame <- data.frame(ylab = seq(plyr::round_any(min(mkt_val)/(1+max(comp)) - max(dev_cost),10,floor),
                                    plyr::round_any(max(mkt_val)/(1+min(comp)) - min(dev_cost),10,ceiling),10))

g3 <- ggplot(mkt_sim[[1]], aes(Profit)) +
  stat_ecdf(geom = 'step', pad = F) +
  labs(y='Cumulative probability', x = 'Profit  ($millions)') +
  coord_flip() + 
  # y axis line
  geom_segment(x = 0, xend = 0, 
               y = min(mkt_sim[[1]]$Profit), yend = max(mkt_sim[[1]]$Profit),
               size = 0.5) +
  # x axis line
  geom_segment(y = 0, yend = 0, 
               x = 0, xend = 1,
               size = 0.5) +
  # x ticks
  geom_segment(aes(y = 0, yend = 1, 
                   x = 0, xend = 0 + .0000001)) +
  # y ticks
  geom_segment(aes(y = 0, yend = 0 + .0000001, 
                   x = -32, xend = 62)) + 
  # labels
  geom_text(data=xlab_frame, aes(x=0, y=xlab, label=xlab),
            vjust=1.5) +
  geom_text(data=ylab_frame, aes(x=ylab, y=0, label=ylab),
            hjust=1.5) +
  ggtitle('Cumulative risk profile') +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank())
g3
