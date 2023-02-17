###   Script to implement EM algorithm
###   Initialise data
Zobs <- c(46, 87, 53, 40, 9, 2)
n <- 237
P <- runif(10, min = 0, max = 1)


###   Initialise the loop
count <- 0
error <- 1
P_est <- rep(0,10)

###   Implement the EM algorithm
for (pi in P) {
  i = match(pi,P)     #index to help store results
    while(error > 10^(-4)){
        gamma <- (1-pi)^6     #   compute quantities from Q which
        Ez_0 <- n*(1-gamma)/gamma
        pi_new <- (Zobs%*%c(1:6))/(12*sum(Zobs) + 6*Ez_0)   # compue estimator for pi
        error <- abs(pi_new - pi)     #  record iteration error
        pi <- pi_new
        
        #   stop if iterations too high
        count <- count+1
        
          if(count > 10000){break
            }
    }
P_est[i] <- pi_new   #save results
}

