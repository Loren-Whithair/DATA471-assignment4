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



reps <- 50


results <- data.frame()

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
    bias.iqr_tilda <- mean(theta.iqr_tilda) - lam 
    
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
      method = c("ML", "IQR", "IQR_tilda"),
      bias = c(bias.ml, bias.iqr, bias.iqr_tilda),
      variance = c(var.ml, var.iqr, var.iqr_tilda),
      MSE = c(mse.ml, mse.iqr, mse.iqr_tilda)
    ))
  }
}

View(results)


