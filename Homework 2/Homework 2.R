#Homework 2

# question 1
# create a binary operator 
"%&%" <- function(a,b){
  paste(as.character(substitute(a)), as.character(substitute(b)))
}
# try examples
Hello %&% World
"Hello" %&% "World"


#question 2
# create a linear regression function 
LinearReg <- function(Y, X){
X <- cbind(c(rep(1, nrow(X))), X)
#X
# check if X is invertible or not 
if(det(X) != 0){
XXt <- crossprod(X)
invertX <- solve(XXt)
beta <- invertX %*% t(X) %*% Y
sigma <- (Y - X %*% beta) * (Y - X %*% beta) / (length(Y) - (ncol(X)-1))
varbeta <- t(sigma) %*% solve(t(X) %*% X)
hat <- X %*% solve(t(X) %*% X) %*% t(X)
fitted <- hat %*% Y
Residuals <- fitted - Y
ResultList <- list("beta" = beta, "sigma" = sigma, "varbeta" = varbeta, "fitted" = fitted, "residuals" = Residuals)
} else {
  break
}
# option to print hat
MyOption <- readline("Would you like to see the result of hat? For yes enter 1. For no enter 0: ")
if (MyOption == 1) {
  ResultList2 <- list("beta" = beta, "sigma" = sigma, "varbeta" = varbeta, "fitted" = fitted, "residuals" = Residuals, "hat" = hat)
  return(ResultList2)
}
return(ResultList)
}

# set all the values 
a = sample(1:30, 30, replace = FALSE)
X = matrix(a, nrow = 6, ncol = 5, byrow = TRUE)
Y <- rexp(nrow(X))

# call function 
LinearReg(Y, X)


#question 3
# create a function 
Box <- function(y, x){
  n = 1000
  U1 <- runif(n, 1)
  U2 <- runif(n, 1)
  x <- sqrt(-2*log(U1)) * cos(2*pi*U2)
  y <- sqrt(-2*log(U1)) * sin(2*pi*U2)
}

# call function 
Myfunction <- Box(y, x)

# generate normally distributed random data 
Distribution <- rnorm(Myfunction, mean = 10, sd = 4)

# create a histgram
hist(Distribution,
     main = "Histgram of normally distributed random data from the Box-Muller method",
     xlab = "Value",
     ylab = "Frequency",
     xlim = c(-5,30),
     ylim = c(0,200),
     col = "red")
