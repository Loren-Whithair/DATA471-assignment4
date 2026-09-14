library(boot)

set.seed(12345)

x <- rexp(11, rate = 1)

x

lambda_iqr <- function(data){log(3) / IQR(data)}
lambda_iqr(x)

# bootstrap <- function(data) {
#   r <- round(runif(length(data), min=1, max=length(data)))
#   data[r]
# }


# 
# x
# bootstrap(x)

boot_output <- rep(0, 200)

for(l in 1:200) {
  boot_output[l] <- lambda_iqr(sample(x, size=11, replace=TRUE))
}

boot_output


lambda_iqr(x)

(2 * lambda_iqr(x)) - (mean(boot_output))


