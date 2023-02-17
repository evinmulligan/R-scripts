
IWLS <- function(X, y, b0, tol, max.iterations){
  
  #initialise
  count <- 0
  log.likelihood <- 0
  err <- 1
  b <- b0
  intercept <- log(mean(y)/(1-mean(y)))
  
  while (err > tol && count < max.iterations){
    eta  = X %*% beta
    mu   = plogis(eta)[,1]
    weights.iter  = mu * (1 - mu)
    W    = diag(s)
    z    = eta + (y - mu)/weights
    b <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z
    
    ll = sum(
      dbinom(
        y,
        prob = plogis(X %*% beta),
        size = 1,
        log  = TRUE
      )
    )
    
    currtol = abs(ll - ll_old)
  }
  
  list(
    beta = b,
    iterations = count,
    tol  = err,
    loglikelihood  = ll,
    weights = plogis(X %*% beta) * (1 - plogis(X %*% beta))
  )
  }
}

