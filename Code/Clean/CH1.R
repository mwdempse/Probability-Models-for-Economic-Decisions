rm(list = ls())

"Authors: Matthew Dempsey & Harrison McKenny"

#install.packages(c("tidyverse","EnvStats"))
library(tidyverse, quietly = T)
library(EnvStats, quietly = T)

# percentile.inc function found in Excel
pcntinc <- function(v,x){
  sorted <- sort(v)
  item <- x*(length(v)-1)+1
  p <- sorted[item]
  return(p)
}

# percentrank.inc found in Excel
percentrank.inc <-  function(x,xo) {length(x[x<= xo])/length(x)} 

#simulation coin toss of fair and unfair coins
coin_toss <- function(n=1,prob=0.5){
  #######################
  # n: number of coin flips
  # prob: probability of getting heads 0.5 is fair
  # output: coin score r and coin flip results based on coin score
  #######################
  r = runif(n)
  flip = ifelse(r < prob, 'Heads','Tails')
  z = cbind.data.frame(r,flip)
  return(z)
}

coin_toss(10,.25)

#simulate sales with variable probability of success for sale 
sales <- function(n=20, sale_prob=0.5){
  #######################
  # n: number of calls per week
  # sale_prob: probability of making sale
  # output: Total number of sales out of n, sale score r, and sale results
  #######################
  r = runif(n)
  sale = ifelse(r < sale_prob, 'Sale','No Sale')
  z = cbind.data.frame(r,sale)
  print(paste0(sum(r < sale_prob),' Total Sales in ',n,' Calls'))
  return(z)
}

sales()

# Iterative sales with variable skill level and probability of success for sale
iter_sales <- function(n=20, hi_skill_prob=0.5, hi_sale_prob = 2/3, iterations=1000, v = TRUE, prob=TRUE){
  
  #######################
  # n: number of calls per week
  # hi_skill_prob: probability of LOW skill 
  # hi_sale_prob: probability of making sale based on High skill 
  # iterations: number of iterations
  # prob: create probability output
  # v: view output after running
  # output: total sales and associated skill level dataset, probability of high skill table for each sales level 
  #######################
  
  # Initialize Results vectors
  high_skill <- vector()
  sum_sales <- vector()
  
  for (i in 1:iterations) {
    
    #initialize number of sales
    sales <- 0
    #Sales difficulty score
    r <- runif(n) 
    #High or low skill salesmen
    c <- ifelse(runif(1)<hi_skill_prob,1-hi_sale_prob,hi_sale_prob)
    
    for (j in 1:n) {
    
        sales <- ifelse(r[j]<c,1,0) + sales
    }
    # Log number of sales out of n
    sum_sales[i] <- sales
    
    # Log skill of generated sales rep
    high_skill[i] <- ifelse(c==hi_sale_prob,1,0)
  }
  

  # Calculate probability of high skill given number of sales 
  if (prob==TRUE) {
    prob_table <- matrix(nrow = n, ncol = 5)
    for (k in 1:n) {
      prob_table[k,1] = k
      is_skill_hi <-c()
      
      for (i in 1:iterations){
        if (sum_sales[i]==k & high_skill[i]==1){
          is_skill_hi[i] <- 1
        } else if (sum_sales[i]==k & high_skill[i]==0){
          is_skill_hi[i] <- 0
        }
        else if(sum_sales[i]!= k) {
          is_skill_hi[i] <- NA
        }
      }
      
      #count number of times benchmark is 'hit'
      freq_total <- sum(!is.na(is_skill_hi))
      freq_hi <- sum(is_skill_hi, na.rm = T)
      
      prob_table[k,2] <- freq_total
      prob_table[k,3] <- freq_hi
      prob_table[k,4] <- freq_total - freq_hi
      prob_table[k,5] <- freq_hi/freq_total
      
    }
    
    colnames(prob_table) <- c('Sales','Count','High_Skill','Low_Skill','P(Skill=High|Sales)')
    
    iter_sales_prob_table <<- as.data.frame(prob_table) 
    
    # Combine results
    z <- cbind(sum_sales, high_skill)
    # save to global environment
    iter_sales_results <<- as.data.frame(z)
    # view data
    if (v == TRUE) {View(iter_sales_results); View(prob_table)}
    
    return(summary(z))
  }
  
 
  
  # Combine results
  z <- cbind(sum_sales, high_skill)
  # save to global environment
  iter_sales_results <<- as.data.frame(z)
  # view data
  if (v == TRUE) {View(iter_sales_results)}
  
  return(summary(z))
}


iter_sales(n=20,hi_skill_prob=0.5,hi_sale_prob = 2/3,iterations=1000)


# plot results of iter_sales

high <- iter_sales_results %>% filter(high_skill ==1) 
high$skill <- 'High Skill'
high$high_skill <- NULL 

low <- iter_sales_results %>% filter(high_skill ==0)
low$skill <- 'Low Skill'
low$high_skill <- NULL 

Total <- iter_sales_results %>% group_by(sum_sales)
Total$skill <- 'Total' 
Total$high_skill <-NULL 

iter_sales_long <- rbind.data.frame(high,low,Total)
                      
ggplot(iter_sales_long,aes(x= sum_sales, fill=skill)) + 
  geom_bar(position='dodge')

# Iterative sales using a triangular distribution
tri_iter_sales <- function(n=20, peak = 0.5, iterations=1000, v = TRUE){
  
  #######################
  # n: number of calls per week
  # peak: peak of triangle distribution
  # iterations: number of iterations
  # prob: create probability output
  # v: view output after running
  # output: total sales and associated skill level dataset, probability of high skill table for each sales level 
  #######################
  
  # Initialize Results vectors
  skill <- vector()
  sum_sales <- vector()
  
  for (i in 1:iterations) {
    
    #initialize number of sales
    sales <- 0
    #Sales difficulty score
    r <- runif(n) 
    # Skill of salesmen
    c <- rtri(1,min=0,max=1,mode=peak)
    # calculate number of sales
    for (j in 1:n) {
      sales <- ifelse(r[j]<c,1,0) + sales
    }
    # Log number of sales out of n
    sum_sales[i] <- sales
    
    # Log skill of generated sales rep
    skill[i] <- c
  }
  
  
  # Calculate rank of skills
  
  z <- cbind.data.frame(sum_sales, skill)
  
  tri_iter_sales_raw_results <<- z %>% 
    arrange(sum_sales) %>%  
    mutate(skill_rank = percent_rank(skill))
  
  tri_iter_sales_results <<- z %>% 
    arrange(sum_sales) %>%  
    mutate(skill_rank = percent_rank(skill)) %>% 
    group_by(sum_sales) %>% 
    summarise(mean_skill = mean(skill),mean_skill_rank = mean(skill_rank), count = n())
     
  if (v == TRUE) {View(tri_iter_sales_raw_results); View(tri_iter_sales_results)}
  
  return()
}


tri_iter_sales()

