rm(list = ls())

"Authors: Matthew Dempsey & Harrison McKenny"

library(tidyverse)

# Load in Simtool specific functions from GITHUB
source('https://raw.githubusercontent.com/mwdempse/Probability-Models-for-Economic-Decisions/main/Code/Clean/Simtool_utility_functions.R')
source('https://raw.githubusercontent.com/mwdempse/prefixMerge/main/prefixMerge.R')

# Fig 4.1 Normal Distribution

norm_dist <- function(mu,stdv) {
   
  q = qnorm(c(.001,seq(.01,.99,.01),.999),mu,stdv)
  d = dnorm(q,mu,stdv)
  p = pnorm(q,mu,stdv)
  
  n_distrubution = cbind.data.frame(p,q,d)
  
  g1 = ggplot(n_distrubution, aes(x = p,y=q)) +
    labs(y = 'Value') +
    ggtitle('Inverse Cumulative Distribution') +
    geom_line(size = 1.2) +
    ylim(0,max(ceiling(q))+5) +
    scale_x_continuous("Cumulative Probability",breaks = round(seq(min(n_distrubution$p), max(n_distrubution$p)+.01, by = 0.1),1)) + 
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(size = 1))
  
  
  g2 = ggplot(n_distrubution, aes(x = q,y=d)) +
    labs(x = "Value") +
    ggtitle('Probability Density') +
    geom_line(size = 1.2) +
    scale_y_continuous('Probability Density',breaks = seq(0, max(n_distrubution$d)+.01, by = 0.01)) +
    xlim(0,max(ceiling(q))+5) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(size = 1))
  
  g3 = ggplot(n_distrubution, aes(x = q,y=p)) +
    labs(x = "Value") +
    ggtitle('Cumulative Distribution') +
    geom_line(size = 1.2) +
    scale_y_continuous('Cumulative Probability',breaks = seq(0, max(n_distrubution$p)+.1, by = 0.1)) +
    xlim(0,max(ceiling(q))+5) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(size = 1))
  
  names(n_distrubution) <- c('CumvProby','Value',	'ProbyDensity')
  
  results <- list(n_distrubution,g1,g2,g3)
  
  names(results) <- c('n_distrubution','Inverse Cumulative Distribution','Probability Density','Cumulative Distribution')
  
  return(results)
}

norm_dist(mu = 26,stdv = 4.5)



#-----------------------------------Figure 4.2 Compound interest and growth rate calculations with EXP and LN--------------
#The EXP Function from Compound Interest:
#Suppose we start with 1$ in a bank account.
b3 <- 0.12 #Nominal interest rate 12%
b4 <- 4 # Times compounded per year (try 1,2,4,12,365)
b5 <- (1+b3/b4)^b4 #$ Value at end of year
b6 <- exp(b3) #EXPonential formula
b6
b7 <- log(b6) #LN, the inverse of EXP
b7

d6 <- exp(1)
d6 #The number e:
e6 <- d6^b3
e6 #e^r

#So EXP(r) is the actual yield, per 1$ initial investment, after a year
#when nominal interest rate r is compounded continuously
#LN(x) is the nominal interest rate that would yeild $x after a year,
#per $1 initial investment, with continuous compounding.

#AN EXAMPLE TO SHOW THE ADVANTAGE OF LOGARITHMIC GROWTH RATES
b16_18 <- c(100,80,100) #Values
#growth rates:
c17 <- (b16_18[2]-b16_18[1])/b16_18[1] #Percent
c18 <- (b16_18[3]-b16_18[2])/b16_18[2]
c17_18 <- c(c17,c18)
c17_18

d17 <- log(b16_18[2]/b16_18[1]) #Logarithmic
d18 <- log(b16_18[3]/b16_18[2])
d17_18 <- c(d17,d18)
d17_18

#Average growth rate:
c19 <- mean(c17_18) #Percent
c19 #2.5%

d19 <- mean(d17_18)
d19 #essentially 0%


#---------------------------Figure 4.3 Multiplication and Addition Properties of EXP and LN--------------
#Other Facts
#EXP(r1+r2) = EXP(r1)*EXP(r2)
#LN(x1*x2) = LN(x1)+LN(x2)

#Example:
h22 <- qnorm(runif(1),0,1) # r (random)
i22 <- qnorm(runif(1),0,1) # s (random)

h24 <- exp(h22) #x = EXP(r)
i24 <- exp(i22) #y = EXP(s)

h26 <- log(h24) #LN(x)
i26 <- log(i24) #LN(y)

k24 <- h24*i24 #EXP(r)*EXP(s)
l24 <- h22+i22 #EXP(r+s)

k26 <- h26+i26 #LN(x)+LN(y)
l26 <- log(h24*i24) #LN(x*y)


# Fig 4.4 Repeated investment strategies yielding payoffs which are Normal or Lognormal


frac_fixed_investment <- function(wealth  = 1000, fixed = 100, frac = 0.1,
                   return = c(0.25,2), n1 = 1000, n2 = 100, prob = NULL, graph = FALSE) {
  
  FixAmt = vector()
  FixFrac = vector()
  
  for (k in 1:n1) {
    retn = sample(return,size = n2, replace = T, prob = prob) 
    
    fixed_sim = frac_sim = wealth
    
    for (i in 2:n2) {
      fixed_sim[i] = fixed_sim[i-1]+fixed*(retn[i]-1)
      frac_sim[i] = frac_sim[i-1]*(1+ frac*(retn[i]-1))
    }
    
    FixAmt[k] = fixed_sim[n2]
    FixFrac[k] = frac_sim[n2]
    
  }
  
  investment_strat = round(data.frame(retn,fixed_sim,frac_sim),2)
  sim_data = data.frame(FixAmt = round(FixAmt,2),FixFrac = round(FixFrac,2),
                        ln_FixFrac = log(FixFrac))
  
  
  
  FixAmt_mean = mean(FixAmt)
  FixAmt_sd = sd(FixAmt)
  FixAmt_q = quantile(FixAmt,c(.25,.5,.75))
  
  FixFrac_mean = mean(FixFrac)
  FixFrac_sd = sd(FixFrac)
  FixFrac_q = quantile(FixFrac,c(.25,.5,.75))
  
  ln_FixFrac_mean = mean(log(FixFrac))
  ln_FixFrac_sd = sd(log(FixFrac))
  ln_FixFrac_q = quantile(sim_data$ln_FixFrac,c(.25,.5,.75))
  Percentile = c(0.01,seq(0.1,0.9,0.1),0.99)
  
  quants = sim_data %>% summarise(FixAmt_final = quantile(FixAmt,Percentile),
                                  FixAmt_NORM = qnorm(Percentile, FixFrac_mean, FixAmt_sd),
                                  FixAmt_GENL = GENLINV(Percentile,quart1 = FixAmt_q[1],
                                                        quart2 = FixAmt_q[2],quart3 = FixAmt_q[3]),
                                  FixFrac_final = quantile(FixFrac,Percentile),
                                  FixFrac_ExpNORM = exp(qnorm(Percentile, ln_FixFrac_mean, ln_FixFrac_sd)),
                                  FixFrac_LNORM = qlnorm(Percentile, ln_FixFrac_mean, ln_FixFrac_sd),
                                  FixFrac_GENL = GENLINV(Percentile,quart1 = FixFrac_q[1],
                                                         quart2 = FixFrac_q[2],quart3 = FixFrac_q[3]))
  quants = cbind.data.frame(Percentile,quants)
  
  
  g1 = ggplot(investment_strat) + 
    geom_line(aes(y = fixed_sim, x = seq(1,100,1), color = 'Fixed Amount'), size = 1) +
    geom_line(aes(y = frac_sim, x = seq(1,100,1), color = 'Fixed Fraction'), size = 1) +
    scale_color_manual("",
                       values = c('Fixed Amount' = 'blue','Fixed Fraction' = 'red')) +
    scale_y_continuous(breaks = seq(-1000,9000,1000),limits = c(-1000, 9000)) +
    labs(x = NULL, y = 'Money in Account') +
    ggtitle('Investment Strategies') +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(size = 1))
  
  g2 = ggplot(quants) + 
    geom_line(aes(y = FixAmt_final, x = Percentile, color = 'Data'), size = 1) +
    geom_line(aes(y = FixAmt_NORM, x = Percentile, color = 'NORM'), size = 1) +
    geom_line(aes(y = FixAmt_GENL, x = Percentile, color = 'GENL'), size = 1) +
    scale_color_manual("",
                       values = c('Data' = 'blue','NORM' = 'red', 'GENL' = 'green')) +
    scale_y_continuous(breaks = seq(-6000,8000,1000),limits = c(-6000, 8000)) +
    labs(x = NULL, y = NULL) +
    ggtitle('Fixed-Amount Inverse Cumulative') +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(size = 1))
  
  g3 = ggplot(quants) + 
    geom_line(aes(y = FixFrac_final, x = Percentile, color = 'Data'), size = 1) +
    geom_line(aes(y = FixFrac_ExpNORM, x = Percentile, color = 'EXP(NORM)'), size = 1) +
    geom_line(aes(y = FixFrac_LNORM, x = Percentile, color = 'LNORM'), size = 1) +
    geom_line(aes(y = FixFrac_GENL, x = Percentile, color = 'GENL'), size = 1) +
    scale_color_manual("",
                       values = c('Data' = 'blue', 'EXP(NORM)' = 'red', 'LNORM' = 'black','GENL' = 'green')) +
    scale_y_continuous(breaks = seq(-10000,60000,10000),limits = c(-10000, 60000)) +
    labs(x = NULL, y = NULL) +
    ggtitle('Fixed-Fraction Inverse Cumulative') +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(size = 1))
  
  
  results = list(investment_strat, sim_data,quants,g1,g2,g3)
  return(results)
  
}

frac_fixed_investment()



# Fig 4.5 Time Diversification Fallacy

TDF <- function(ln_mean= 0.04, ln_sd = 0.2,
                inv_per = c(1,5,10,20), n = 1000) {
  
  sgr_sim = matrix(nrow = n, ncol = length(inv_per))
  slr_sim = matrix(nrow = n, ncol = length(inv_per))
  
  for (i in 1:n) {
    
    # Simulated Growth Ratios	
    sgr = rlnorm(n = max(inv_per), ln_mean, ln_sd)
    # Simulated Logarithmic Returns
    slr = log(sgr)
    
    for (k in 1:length(inv_per)) {
      
      sgr_sim[i,k] = prod(sgr[1:inv_per[k]])
      slr_sim[i,k] = mean(slr[1:inv_per[k]])
      
    }  
  }
  
  sgr_sim = as.data.frame(sgr_sim)
  slr_sim = as.data.frame(slr_sim)
  
  sim = cbind.data.frame(sgr,slr)
  
  sgr_sum_stat = sgr_sim %>% summarise(average = colMeans(sgr_sim), sd = sapply(sgr_sim, sd),
                                       five_per = sapply(sgr_sim, quantile, probs = 0.05))
  
  five_cond_tail = c()
  
  for (j in 1:length(inv_per)) {
    five_cond_tail[j] = mean(sgr_sim[sgr_sim[,j] <= sgr_sum_stat[j,3],j])
  }
  
  sgr_sum_stat = cbind.data.frame(sgr_sum_stat,five_cond_tail)
  
  slr_sum_stat = slr_sim %>% summarise(average = colMeans(slr_sim), sd = sapply(slr_sim, sd),
                                       five_per = sapply(slr_sim, quantile, probs = 0.05))
  five_cond_tail = c()
  
  for (j in 1:length(inv_per)) {
    five_cond_tail[j] = mean(slr_sim[slr_sim[,j] <= slr_sum_stat[j,3],j])
  }
  
  slr_sum_stat = cbind.data.frame(slr_sum_stat,five_cond_tail)
  
  
  rnames = outer('n',inv_per, paste, sep = '=')
  
  rownames(sgr_sum_stat) = rnames 
  rownames(slr_sum_stat) = rnames 
  tsgr_sum_stat = t(sgr_sum_stat)
  tslr_sum_stat = t(slr_sum_stat)
  sum_stat = prefixMerge(tsgr_sum_stat, tslr_sum_stat, by=0, all=TRUE, prefix = c("sgr_","slr_")) %>% column_to_rownames(var="Row.names")
  
  colnames(sgr_sim) = rnames
  colnames(slr_sim) = rnames
  
  results = list(sim,sgr_sim,slr_sim,sum_stat)
  names(results) = c('Simulation','Total Return','Annualized Returns','Summary Statistics')
  return(results)
  
}

tdf <- TDF()
View(tdf)

# Fig 4.6 & 4.7 Annualized and Total Returns by Investment Horizon 

AROI <- tdf[['Annualized Returns']]

plt_AROI <- gather(AROI) %>%
  separate(key,'n=', remove = F, into = c('blank','order'), convert = T) %>%
  mutate(key2 = factor(key,levels = reorder(unique(key),unique(order)))) %>% 
  select(-c(blank,order,key))
  
  
ggplot(plt_AROI, aes(x = value, fill = key2)) + 
  geom_histogram(bins = 30) + 
  labs(title = 'Annualized Returns by Investment Horizon',
       subtitle = 'with free x axis',
       y = 'Frequency') +
  facet_wrap(~key2,scales = 'free_x')

TROI <- tdf[['Total Return']]
plt_TROI <- gather(TROI) %>%
  separate(key,'n=', remove = F, into = c('blank','order'), convert = T) %>%
  mutate(key2 = factor(key,levels = reorder(unique(key),unique(order)))) %>% 
  select(-c(blank,order,key))


ggplot(plt_TROI, aes(x = value, fill = key2)) + 
  geom_histogram(bins = 30) + 
  labs(title = 'Total Returns by Investment Horizon',
       subtitle = 'with free x axis',
       y = 'Frequency') +
  facet_wrap(~key2, scales = 'free_x')


# Fig 8-11 left to add
