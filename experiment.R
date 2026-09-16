# set.seed(123456)

x <- rexp(11, rate = 1)

x

lambda_iqr <- function(data){log(3) / IQR(data)}
lambda_iqr(x)

boot_output <- rep(0, 200)

for(l in 1:200) {
  boot_output[l] <- lambda_iqr(sample(x, size=11, replace=TRUE))
}

boot_output


lambda_iqr(x)

(2 * lambda_iqr(x)) - (mean(boot_output))


### monte carlo

set.seed(1234567)

reps <- 20 # the number of replications per lambda, n combo

results <- data.frame()

invalid_lambda_tilda_reps <- data.frame(
  A=NA,
  B=NA
)

issues[-1,]

issues <- data.frame()

rbind(issues, c("A"=1, "B"=2))



for (lam in c(.1,1,10)){ #all lambda (true rate of the exponential) we are testing
  for (n in c(5,30,100)){# all sample sizes we are testing
    theta.ml <- rep(NA, reps)
    theta.iqr <- rep(NA, reps)
    theta.iqr_tilda <- rep(NA, reps)
    for (r in 1:reps){
      x <- rexp(n, rate=lam)
      theta.ml[r] <- lambda_ml(x)
      theta.iqr[r] <- lambda_iqr(x)
      theta.iqr_tilda[r] <- lambda_tilda(x) # lambda_iqr adjusted for bias with bootstrapping
      
    }
    
    # calculate the bias of each theta, B
    # - uses law of large numbers (LLM) to approximate expected value as mean of multiple samples
    # - lam is the true statistics that we are trying to estimate
    bias.ml <- mean(theta.ml) - lam 
    bias.iqr <- mean(theta.iqr) - lam 
    
    # remove
    theta.iqr_tilda[theta.iqr_tilda == Inf] <- NA
    bias.iqr_tilda <- mean(theta.iqr_tilda, na.rm=TRUE) - lam 
    
    # calculate the variance of each theta
    var.ml <- var(theta.ml)
    var.iqr <- var(theta.iqr)
    var.iqr_tilda <- var(theta.iqr_tilda)
    
    # calculate the MSE
    mse.ml <- bias.ml^2 + var.ml
    mse.iqr <- bias.iqr^2 + var.iqr
    mse.iqr_tilda <- bias.iqr_tilda^2 + var.iqr_tilda
    
    # store the values
    results <- rbind(results, data.frame(
      lambda = lam,
      n = n,
      MSE_ml = mse.ml,
      MSE_iqr = mse.iqr,
      MSE_iqr_tilda = mse.iqr_tilda
    ))
    
    invalid_lambda_tilda_reps <- rbind(invalid_lambda_tilda_reps, data.frame(
      lambda = lam,
      n = n,
      invalid_reps = inf_reps
    ))
  }
}
View(results)




set.seed(1234567)

reps <- 100 # the number of replications per lambda, n combo

results <- data.frame()



x <- rexp(6, rate=1)

lambda_tilda(x)

for (lam in c(.1,1,10)){ #all lambda (true rate of the exponential) we are testing
  for (n in c(5,30,100)){# all sample sizes we are testing
    theta.ml <- rep(NA, reps)
    theta.iqr <- rep(NA, reps)
    theta.iqr_tilda <- rep(NA, reps)
    for (r in 1:reps){
      x <- rexp(n, rate=lam)
      theta.ml[r] <- lambda_ml(x)
      theta.iqr[r] <- lambda_iqr(x)
      theta.iqr_tilda[r] <- lambda_tilda(x) # lambda_iqr adjusted for bias with bootstrapping
      
    }
    

        print(sum(abs(theta.iqr_tilda)==Inf))
    print("IQR tilda values:")
    print(theta.iqr_tilda)
    print("======")
    # Preparing infinite values for exclusion
    theta.iqr_tilda[abs(theta.iqr_tilda)==Inf] <- NA
    lambda_tilda.infs <- sum(is.na(theta.iqr_tilda))
    print(sum(theta.iqr_tilda))
    
    print(paste0("For n=", n, ",  lambda=", lam, ", of ", reps, "reps, ", lambda_tilda.infs, " of them were Inf"))
    
    # calculate the bias of each theta, B
    # - uses law of large numbers (LLM) to approximate expected value as mean of multiple samples
    # - lam is the true statistics that we are trying to estimate
    
    bias.ml <- mean(theta.ml) - lam
    bias.iqr <- mean(theta.iqr) - lam 
    bias.iqr_tilda <- mean(theta.iqr_tilda, na.rm=TRUE) - lam
    
    # calculate the variance of each theta
    var.ml <- var(theta.ml)
    var.iqr <- var(theta.iqr)
    var.iqr_tilda <- var(theta.iqr_tilda, na.rm=TRUE)
    
    # calculate the MSE
    mse.ml <- bias.ml^2 + var.ml
    mse.iqr <- bias.iqr^2 + var.iqr
    mse.iqr_tilda <- bias.iqr_tilda^2 + var.iqr_tilda
    
    # store the values
    results <- rbind(results, data.frame(
      lambda = lam,
      n = n,
      MSE_ml = mse.ml,
      MSE_iqr = mse.iqr,
      MSE_iqr_tilda = mse.iqr_tilda,
      inf_values_iqr_tilda = lambda_tilda.infs
    ))
  }
}

View(results)


lambda_tilda(c(4,4,4,4)) == Inf
