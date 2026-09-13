# computes the lambda_iqr estimators
lambda_iqr <- function(X){
  q <- quantile(X)
  return (log(3) / (q["75%"] - q["25%"]))
}

X <- c(1,1,2,3,3,4,8,8,10)
lambda_iqr(X)
typeof(quantile(X)[5])


q <- quantile(X)

log(3) / (q["75%"] - q["25%"])

typeof((q["75%"] - q["25%"]))

typeof(lambda_iqr(X))
