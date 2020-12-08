rm(list = ls())

"Authors: Matthew Dempsey & Harrison McKenny"

# Packages
# install.packages(c("tidyverse","EnvStats","data.tree","stringr"))
library(tidyverse, quietly = T)
library(EnvStats, quietly = T)
library(data.tree, quietly = T)
library(stringr, quietly = T)

# Load in Simtool specific functions from GITHUB
source('https://raw.githubusercontent.com/mwdempse/Probability-Models-for-Economic-Decisions/main/Code/Clean/Simtool_utility_functions.R')

# Fig 1.1/1.2 Simulation of coin toss with adjustable probability

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

# Fig 1.3 Simple model of independent sales calls with variable probability of success for sale 

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

# Fig 1.6 & 1.7 Iterative sales with variable skill level and probability of success for sale

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


# Fig 1.8 Frequency plot of sales from simulation by skill level

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


# Fig 1.9 Iterative sales with variable skill level and probability of success for sale using a triangular distribution
 
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
  
  tri_iter_sales_raw_results <- z %>% 
    arrange(sum_sales) %>%  
    mutate(skill_rank = percent_rank(skill))
  
  tri_iter_sales_results <- z %>% 
    arrange(sum_sales) %>%  
    mutate(skill_rank = percent_rank(skill)) %>% 
    group_by(sum_sales) %>% 
    summarise(mean_skill = mean(skill),mean_skill_rank = mean(skill_rank), count = n())
     
  if (v == TRUE) {View(tri_iter_sales_raw_results); View(tri_iter_sales_results)}
  
  results = list(tri_iter_sales_raw_results,tri_iter_sales_results)
  
  return(results)
}


tri_iter_sales()


#Fig 1.10 & 1.11 Simulation and probability tree for oil exploration example

data_for_tree <- function(pathString, prob, start_node = "Oil State" ){
  
  #######################
  # pathString: vector of strings in path order separated by '/'
  # prob: vector of probabilities which corresponds to the order of the pathString
  # start_node: label of the starting node
  # output: dataset for the plot_tree, or given_prob functions
  #######################  
  
  prob_data <- cbind.data.frame(pathString,prob) %>%
    mutate(tree_level = str_count(string = pathString,
                                  pattern = "/") + 1,
           tree_group = str_replace(string = pathString, pattern = "/.*", replacement = ""),
           node_type = "decision_node")
  
  max_tree_level <- max(prob_data$tree_level, na.rm = T) 
  
  # get distinct probabilities to find parent node probability
  parent_lookup <- prob_data %>% distinct(pathString, prob) 
  
  # Loop through all tree layers to get all immediate parent probabilities (to calculate cumulative prob)
  for (i in 1:(max_tree_level -  1)) { 
    
    names(parent_lookup)[1] <-paste0("parent",i)
    names(parent_lookup)[2] <-paste0("parent_prob",i)
    
    for (j in 1:i) {
      
      if (j == 1)  prob_data[[paste0("parent",i)]] <- sub("/[^/]+$", "", prob_data$pathString)
      else if (j  > 1) prob_data[[paste0("parent",i)]] <- sub("/[^/]+$", "", prob_data[[paste0("parent",i)]])
    }
    
    prob_data <- prob_data %>% left_join(parent_lookup, by = paste0("parent",i))
    
  }
  
  # calculate cumulative probability
  prob_data$overall_prob <- round(apply(prob_data %>% select(contains("prob")), 1, prod, na.rm = T),4)  
  
  # create new rows that will display terminal/final step calculations on the tree
  terminal_data <- prob_data %>%  filter(tree_level == max_tree_level) %>%
    mutate(node_type = 'terminal',
           pathString = paste0(pathString, "/overall"),
           prob = NA,
           tree_level = max_tree_level + 1)
  
  # bind everything together 
  prob_data = bind_rows(prob_data, terminal_data) %>%  
    mutate(pathString = paste0(start_node,"/",pathString),
           overall_prob = ifelse(node_type == 'terminal', overall_prob, NA),
           prob_rank = rank(-overall_prob, ties.method = "min", na.last = "keep"))
  
  # add row to serve as the start node label
  prob_data = bind_rows(prob_data, data.frame(pathString = start_node, node_type = 'start', tree_level = 0)) %>% 
    select(-contains("parent"))
  return(prob_data)
  
}

# Probability tree plotting function

plot_tree <- function(df, direction = "LR") {
  
  #######################
  # df: dataframe from data_for_tree function
  # direction: direction tree grows
  # output: probability tree
  #######################  
  
  # convert data to nodes 
  tree_plot <- as.Node(df) 
  
  EdgeLabel <- function(node) switch(node$node_type, node$prob)
  
  NodeShape <- function(node) switch(node$node_type, start = "box", node_decision = "circle", terminal = "none")
  
  NodeLabel <- function(node) switch(node$node_type, terminal = paste0("Prob: ", node$overall_prob),node$node_name)
  
  SetEdgeStyle(tree_plot, fontname = 'helvetica', label = EdgeLabel)
  
  SetNodeStyle(tree_plot, fontname = 'helvetica', label = NodeLabel, shape = NodeShape)
  
  SetGraphStyle(tree_plot, rankdir = direction) 
  
  plot(tree_plot)
  
}

pathString <- c('Oil','Oil/A','Oil/A/B','Oil/A/noB',
                'Oil/noA','Oil/noA/B','Oil/noA/noB',
                'noOil','noOil/A','noOil/A/B','noOil/A/noB',
                'noOil/noA','noOil/noA/B','noOil/noA/noB')

prob <- c(0.6,0.3,0.3,0.7,
          0.7,0.3,0.7,
          0.4,0.1,0.1,0.9,
          0.9,0.1,0.9)

oil_tree_data <- data_for_tree(pathString = pathString, prob = prob)
plot_tree(oil_tree_data)





# Fig 1.12 Alternative tree and model for oil exploration example
given_prob <- function(df) {
  
  #######################
  # df: dataframe from data_for_tree function
  # output: dataframe to be used by plot_tree function
  #######################  
  
  df2 <- df %>% 
    filter(node_type == 'terminal') %>% 
    select(pathString,overall_prob)
  df3<- df2 %>%
    mutate(pathString = str_remove(df2$pathString,'Oil State/Oil|Oil State/noOil|Oil State'))
  
  # probability of there being oil in A
  prob_A <- as.numeric(df3 %>% 
                         filter(pathString %in% c(str_subset(df3$pathString,"/A"))) %>%
                         summarise(sum = sum(overall_prob)))
  
  # probability of oil in B and A
  prob_A_and_B <- as.numeric(df3 %>% 
                               filter(pathString %in% c(str_subset(df3$pathString,paste0("/A","/B")))) %>% 
                               summarise(sum = sum(overall_prob)) %>%
                               round(4))
  
  # probability of oil in B given oil in A
  prob_B_given_A <- round(prob_A_and_B/prob_A,4)
  
  # probability of no oil in B given oil in A
  prob_not_B_given_A <- 1-prob_B_given_A
  
  
  # probability of there not being oil in A                  
  prob_not_A <- 1-prob_A
  
  # probability of there being oil in B and no oil in A
  prob_B_and_not_A <- as.numeric(df3 %>% 
                                   filter(pathString %in% c(str_subset(df3$pathString,paste0("/noA","/B")))) %>% 
                                   summarise(sum = sum(overall_prob)))
  
  # probability of there being oil in B given no oil in A
  prob_B_given_not_A <- round(prob_B_and_not_A/prob_not_A,4)
  
  # Probability of there being no oil in B given no oil in A
  prob_not_B_given_not_A <- 1 - prob_B_given_not_A 
  
  
  given_pathString <<- c('A','A/B','A/noB',
                         'noA','noA/B','noA/noB')
  given_prob <<- as.numeric(lapply(c(prob_A,prob_B_given_A,prob_not_B_given_A,
                                     prob_not_A,prob_B_given_not_A,prob_not_B_given_not_A),round,4))
  
  return(cbind(given_pathString,given_prob))
}

given_prob(oil_tree_data)
given_tree_data <- data_for_tree(pathString = given_pathString, prob = given_prob, start_node = 'X')
plot_tree(given_tree_data)

# Fig 1.13 Binomial Probability computations for sales person example

iter_sales_binom <- function(n=20, pskill=0.5,psale=2/3, v = TRUE) {
  
  #######################
  # Note: uses a binomial distribution instead of relying on simulating the number of sales
  # n: number of sales
  # probskill: probability of HIGH skill 
  # psale: probability of making sale based on High skill 
  # v: view output after running
  # output: list of dataframes of probability of sales given skill level and probability of skill level given number of sales
  #######################
  
  num_sales = seq(0,n,1)
  
  # High skill
  p_sale_given_hi_skill = dbinom(num_sales,n,psale)
  product_hi = pskill*p_sale_given_hi_skill
  
  # Low skill
  p_sale_given_low_skill = dbinom(num_sales,n,1-psale)
  product_low = (1-pskill)*p_sale_given_low_skill
  
  p_hi_skill_given_num_sales=c()
  p_low_skill_given_num_sales=c()
  
  # probability of skill given number of sales
  for (i in 1:(n+1)) {
    p_hi_skill_given_num_sales[i] = product_hi[i]/(product_hi[i] + product_low[i])
    p_low_skill_given_num_sales[i] = product_hi[i]/(product_hi[i] + product_low[i])
  }
  
  high_skill = round(cbind.data.frame(num_sales,p_sale_given_hi_skill,product_hi,p_hi_skill_given_num_sales),4)
  low_skill = round(cbind.data.frame(num_sales,p_sale_given_low_skill,product_low,p_low_skill_given_num_sales),4)
  
  result = list(`high skill`=high_skill,`low skill`=low_skill)
  
  # view data
  if (v == TRUE) {View(result[['high skill']]); View(result[['low skill']])}
  
  return(result)
  
  
}

iter_sales_binom()

# Fig 1.14 Compution frequencies of differend outcomes in oil exploration example

oil_sim <- function(n = 1000, poil = 0.6, pA = 0.3, pB = 0.3, pnA = 0.1, pnB = 0.1) {
  
  #######################
  # n: number of simulations
  # poil: probability of oil rich world state 
  # pA: probability of oil in A given oil rich world state 
  # pB: probability of oil in B given oil rich world state 
  # pnA: probability of oil in A given oil poor world state 
  # pnB: probability of oil in A given oil poor world state 
  # output: list of dataframes of probability of sales given skill level and probability of skill level given number of sales
  #######################
  
  
  oil <- ifelse(runif(n) < poil,1,0)
  A <- ifelse(runif(n) < ifelse(oil==1,pA,pnA),1,0)
  B <- ifelse(runif(n) < ifelse(oil==1,pB,pnB),1,0)
  
  Simulation_Model <- data.frame(oil,A,B)
  
  # unique patterns found in the simulation
  Pattern_Matrix <- unique(Simulation_Model)
  
  
  Frequency <- Simulation_Model %>%
    mutate(pattern = paste0(oil,A,B)) %>%
    group_by(pattern) %>%
    summarize(Frequency= n())# %>%
  # select(Frequency)
  
  Frequency_Matrix <- Pattern_Matrix %>% 
    mutate(pattern = paste0(Pattern_Matrix[,1],Pattern_Matrix[,2],Pattern_Matrix[,3]),
           oil = ifelse(oil == 1,"Yes","No"),
           A = ifelse(A == 1,"Yes","No"),
           B = ifelse(B == 1,"Yes","No")) %>% 
    inner_join(Frequency) %>%
    select(-pattern) %>% 
    arrange(desc(oil),desc(A), desc(B))
  
  colnames(Frequency_Matrix) <- c("FavStr?","Oil@A?","Oil@B?","Frequency")
  
  results <- list(Simulation_Model, Pattern_Matrix, Frequency_Matrix)
  
  return(results)
  
}

oil_sim()
