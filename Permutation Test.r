#--- Created By Dustin Smith on 1/25/2024 ---#
#--- Edited on 2/7/2024                   ---#

### This script is used to preform a Permutation Test on a set of samples

### Here are some functions that will be used below:
  ## This is for calculating a t_test
  t_test <- function(means,sigma,n){
    df <- 5
    standard_pooled <- ((n[1] - 1) * (sigma[1] ^ 2) + (n[2] - 1) * (sigma[2] ^ 2 )) / (n[1] + n[2] - 2)
    standard_error_combined <- sqrt(standard_pooled) * sqrt( 1 / n[1] + 1 / n[2])

    # The produces the initial statistics
    t_statistic <- (diff(-means) - 0) / standard_error_combined
    treatement_diffs <- diff(-means)

    return(data.frame(t_statistic = t_statistic, treatement_diffs = treatement_diffs))
  }

  ## This function will produce the means and sds of each treatment type.
  find_measures <- function(data, treatments){
    for(i in 1:treatments){
      # Here I am assigning measures to each treatment.
      means[i] <- mean(data$response[which(data$treatment == i)])
      sigma[i] <- sd(data$response[which(data$treatment == i)])
      n[i] <- length(data$response[which(data$treatment == i)])
    }

    return(data.frame(means = means, sigma = sigma, n = n))
  }




####------------------------- Attempt 1 --------------------------------####

  ## I am setting initial values here
  data <- data.frame(treatment = c(2,2,1,1,2,1,1), response = c(14,16,19,17,15,13,17))

  means <- c(0,0)
  sigma <- c(0,0)
  n <- c(0,0)

  ## Calculate initial t-test of given sample
  measures <- find_measures(data,treatments = 2)
  perm_table <- t_test(means = measures$means, sigma = measures$sigma, n = measures$n)

  ## Preform "Boot-legging" of given sample, by reshuffling the values and producing new t-values
  fixed_data <- data.frame(treatments = c(rep(1,measures$n[1]),rep(2,measures$n[2])), response = data$response)
  m <- measures$n[1]

  ## This function is designed to shuffle the placements, and find the new test measures
  reps <- 1000
  for(i in 1:reps){
    fixed_data[,1] <- rep(2,nrow(data))
    # I only sampled the location of the first treatment.
    fixed_data[c(sample(1:nrow(data),m)),1] <- 1
    measures <- find_measures(fixed_data,treatments = 2)
    perm_table[i + 1,] <- t_test(means = measures$means, sigma = measures$sigma, n = measures$n)

  }

  ## This loop is to iterate through the found statistics and find the percentage more extreme then the original.
  count <- 0
  for(i in 2:nrow(perm_table)){ #Not this begins at 2, as iteration one is the original to compare.
    if(abs(perm_table$t_statistic[i]) >= perm_table$t_statistic[1]){
      count <- count + 1
    }
  }

  (permutation_statistic <- (count)/(nrow(perm_table) - 1))

  2*(1-pt(perm_table[1,1], 5))




  ####------------------------- Attempt 2 --------------------------------####

  ## I am setting initial values here
  data <- data.frame(treatment = c(2,2,1,1,2,1,1), response = c(14,16,19,17,15,13,17))

  means <- c(0,0)
  sigma <- c(0,0)
  n <- c(0,0)

  ## Calculate initial t-test of given sample
  measures <- find_measures(data,treatments = 2)
  perm_table <- t_test(means = measures$means, sigma = measures$sigma, n = measures$n)

  ## The below method should find all possible permutations of a list of 7 ones and 7 twos.
  ## It is however not restricted to exactly 4 ones and 3 twos.
  perms <- expand.grid(lapply((1:7), function(i) c(2,1)))
  ## Here I have filtered out the permutations to the ones that contain the desired 4 ones, and
  ## 3 twos. It does so by testing the sums to be 4 + 3*2 = 10.
  perms <- perms[which(rowSums(perms) == 10),]
  perm_data <- (as.matrix(perms))

  ## Compare test value to two-way t-test of original experiment.
  for(i in 1:nrow(perm_data)){
    data <- data.frame(treatment = perm_data[i,], response = c(14,16,19,17,15,13,17))
    measures <- find_measures(data,treatments = 2)
    perm_table[i+1,] <- t_test(means = measures$means, sigma = measures$sigma, n = measures$n)
  }

  count <- 0
  for(i in 2:nrow(perm_table)){ #Not this begins at 2, as iteration one is the original to compare.
    if(abs(perm_table$t_statistic[i]) >= perm_table$t_statistic[1]){
      count <- count + 1
    }
  }

  (permutation_statistic <- (count)/(nrow(perm_table) - 1))

  2*(1-pt(perm_table[1,1], 5))

