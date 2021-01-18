set.seed(103031)

## Bayesian calculation if clears 50+1 based on 20,000 simulations

Outcome <- function(mu, tau, bias_sd, avg, sd){
  sigma = sqrt(sd^2 + bias_sd^2) 
  B = ((sigma^2) / (sigma^2 + tau^2)) 
  posterior_mean = (B*mu + (1-B)*avg) 
  posterior_se = (sqrt( 1 / ((1/sigma^2) + (1/tau^2))))  
  NPP <- replicate(50000, {
    results_beta = (rnorm(1, posterior_mean, posterior_se)) 
    AA = (ifelse(results_beta > 0.50, 1, 0)) 
    mean(AA) 
    
  })
  answer <- ifelse(NPP > 0, 1, 0) 
  answer 
}

## SCENARIO 1: Round Two OG

mu <- 0.52125 # Historical Mean
tau <- 0.06 # Historical SE
bias_sd <- 0.0126964562 # Historical Bias SD
avg <- 0.6312 # Current Mean
sd <- 0.0396 # Current SD

## Run Simulations

Results <- Outcome(mu, tau, bias_sd, avg, sd)

## Sort victory simulations

b <- c(1:50000)
Results <- data.frame(b, Results)

Win_Results <- sum(Results$Results > 0)

## calculate win percent

win_percent <- (Win_Results / 50000)

win_percent