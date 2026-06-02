## Likelihood for Structure S0 given the data
## (i.e., given a specific contingency table)
## (Equation A2)
##
## Input: "data", a contingency table with
##           data[1] = N(C=0, E=0)
##           data[2] = N(C=0, E=1)
##           data[3] = N(C=1, E=0)
##           data[4] = N(C=1, E=1)
##
##        "theta", a set of parameter vectors (samples from prior)
##           theta[,1] = b_c (base rate of cause)
##           theta[,2] = w_c (causal strength)
##           theta[,3] = w_a (strength of background) 
##

##

likelihood_data_S0 <- function(data, theta)
{
  ((1-theta[,1])*(1-theta[,3]))^data[1]*
    ((1-theta[,1])*theta[,3])^data[2]*
    (theta[,1]*(1-theta[,3]))^data[3]*
    (theta[,1]*theta[,3])^data[4]
}

## Likelihood for Structure S1 given the data
## (i.e., given a specific contingency table)
## (Equation A3)
##
## Input: "data", a contingency table with
##           data[1] = N(C=0, E=0)
##           data[2] = N(C=0, E=1)
##           data[3] = N(C=1, E=0)
##           data[4] = N(C=1, E=1)
##
##        "theta", a set of parameter vectors (samples from prior)
##           theta[,1] = b_c (base rate of cause)
##           theta[,2] = w_c (causal strength)
##           theta[,3] = w_a (strength of background) 
##
likelihood_data_S1 <- function(data, theta)
{
  ((1-theta[,1])*(1-theta[,3]))^data[1]*
    ((1-theta[,1])*theta[,3])^data[2]*
    (theta[,1]*(1-theta[,2])*(1-theta[,3]))^data[3]*
    ((theta[,2]+theta[,3]-theta[,2]*theta[,3])*theta[,1])^data[4]
}


## MAIN FUNCTION - generates SI predictions
## 
## Input: "data", a set of contingency tables (rows) with
##           data[,1] = N(C=0, E=0)
##           data[,2] = N(C=0, E=1)
##           data[,3] = N(C=1, E=0)
##           data[,4] = N(C=1, E=1)
##
##        "m", number of bootstrap samples to be drawn
##
## Output: a matrix with columns:
##           N_00    = N(C=0, E=0), corresponding to data[,1]
##           N_01    = N(C=0, E=1), corresponding to data[,2]
##           N_10    = N(C=1, E=0), corresponding to data[,3]
##           N_11    = N(C=1, E=1), corresponding to data[,4]
##           wc_MLE  = causal power according to data
##           bc_MLE  = base rate of cause according to data
##           wa_MLE  = base rate of effect according to data
##           pEC_MLE = predictive probability P(E|C) according to data
##           pCE_MLE = diagnostic probability P(C|E) according to data (i.e., simple Bayes)
##		 S0_pp   = posterior probabilty of Struture S0 given data
##		 S1_pp   = posterior probabilty of Struture S1 given data
#

## MAIN FUNCTION
generate_SI_preds <- function(data, m, 
                              prior_S0 = 0.5, prior_S1 = 0.5,
                              # Default to the values previously hardcoded
                              bc_prior = c(80, 80), 
                              wc_prior = c(1, 1), 
                              wa_prior = c(40, 5)) 
{
  pred <- matrix(data=0, nrow = dim(data)[1], ncol=11,
                 dimnames = list(1:dim(data)[1],
                                 c("N_00", "N_01", "N_10", "N_11",
                                   "wc_MLE", "bc_MLE", "wa_MLE",
                                   "pEC_MLE", "pCE_MLE",
                                   "S0_pp", "S1_pp")))
  pred <- as.data.frame(pred)
  
  # Map contingency data
  pred$N_00 <- data[,1]; pred$N_01 <- data[,2]
  pred$N_10 <- data[,3]; pred$N_11 <- data[,4]
  
  # MLE Calculations
  pred$wc_MLE  <- (data[,4]/(data[,3]+data[,4]) - data[,2]/(data[,1]+data[,2]))/(data[,1]/(data[,1]+data[,2]))
  pred$bc_MLE  <- rowSums(data[,3:4])/rowSums(data[,1:4])
  pred$wa_MLE  <- data[,2]/(data[,1]+data[,2])
  pred$pEC_MLE <- data[,4]/(data[,3]+data[,4])
  pred$pCE_MLE <- data[,4]/(data[,2]+data[,4])
  
  # Draw samples using the passed parameters
  theta_S1 <- matrix(data=0, nrow=m, ncol=3)
  theta_S1[,1] <- rbeta(m, bc_prior[1], bc_prior[2]) 
  theta_S1[,2] <- rbeta(m, wc_prior[1], wc_prior[2])   
  theta_S1[,3] <- rbeta(m, wa_prior[1], wa_prior[2])   
  
  theta_S0 <- matrix(data=0, nrow=m, ncol=3)
  theta_S0[,1] <- rbeta(m, bc_prior[1], bc_prior[2]) 
  theta_S0[,2] <- rep(0, m)                          
  theta_S0[,3] <- rbeta(m, wa_prior[1], wa_prior[2]) 
  
  for (i in 1:dim(data)[1]) {
    pred$S0_pp[i] <- mean(likelihood_data_S0(data[i,], theta_S0)) * prior_S0
    pred$S1_pp[i] <- mean(likelihood_data_S1(data[i,], theta_S1)) * prior_S1
  }
  
  pred[,c("S0_pp", "S1_pp")] <- pred[,c("S0_pp", "S1_pp")]/rowSums(pred[,c("S0_pp", "S1_pp")])
  return(pred)
}