load("00939843.RData")

M_1 <- function(){
X <- cbind(1, mydat$x)
Y <- mydat$y
mylm <- lm(Y ~ X)
myres <- rstandard(mylm)
plot(mydat$x, myres, ylab="Standardised residuals", xlab="x")
abline(0,0)
}

M_2 <- function(){
  X <- cbind(1, log(mydat$x))
  Y <- mydat$y
  mylm <- lm(Y ~ X)
  myres <- rstandard(mylm)
  plot(log(mydat$x), myres, ylab="Standardised residuals", xlab="log(x)")
  abline(0,0)
}

M_3 <- function(){
  X <- cbind(1, mydat$x)
  Y <- mydat$y
  mylm <- lm(log(Y) ~ X)
  myres <- rstandard(mylm)
  plot(mydat$x, myres, ylab="Standardised residuals", xlab="x")
  abline(0,0)
}

M_4 <- function(){
  X <- cbind(1, log(mydat$x))
  Y <- mydat$y
  mylm <- lm(log(Y) ~ X)
  myres <- rstandard(mylm)
  plot(log(mydat$x), myres, ylab="Standardised residuals", xlab="x")
  abline(0,0)
  
}

X <- mydat$x
Y <- mydat$y
myglm <- glm(Y ~ X, family = gaussian)
summary(myglm)




beta <- c(0,0)
x <- mydat$x
y <- mydat$y
r <- 3
for(i in 1:25){
  
  eta <- cbind(1, log(x)) %*% beta #estimated linear production
  mu <- exp(eta) # estimated mean response
  z <- eta + ((y - mu)/mu) #form the adjusted variate
  w <- (mu * r /(mu + r)) #weights
  lmod <- lm(z~x, weights = w) #regress z on x with weights w
  beta <- as.numeric(lmod$coeff) # new beta
  
}

library(MASS)
summary(m1 <- glm.nb(y ~ log(x), data = mydat, link=log, init.theta = 1))

#loglikelihood check page 80 for AIC formula
b_theta <- mu
theta <- cbind(1, log(x)) %*% beta
sum <- 0
for(i in 1:length(y)){
  
  sum <- sum - b_theta[i] + y[i] * theta[i] + log(choose(y[i] + r - 1, y[i]))
  
}

loglike <-  sum

pglm <- glm(y~log(x), family=poisson)

#var(20 *beta_1 - 17 * beta_2) (actually change this, dont forget covariance)
I49 <- diag(49)
diag(I49) <- w
W <- I49
cov_beta <- solve(t(cbind(1, log(x))) %*% W %*% cbind(1,log(x)))
ans <- 400 * cov_beta[1,1] + 17*17*cov_beta[2,2] - 17*20*cov_beta[2,1]


#pearson's residuals against linear predictor
myres <- y - eta
plot(eta, myres, ylab="residuals", xlab="fitted values")




#question 3
pglm <- glm(y~log(x), family=poisson)
pcoeff<- as.numeric(pglm$coeff)
pnorm((pcoeff[1] - 0.843)/sqrt(cov_beta[1,1]))
my.po.2 <- glm(y~log(x)+I(log(x)^2)+I(log(x)^3),data=mydat,family=poisson)

#chi has a chi squared distribution with 2 degrees of freedom
#under the null hypothesis that M2 is the true model, 
#the difference between the deviances for the two models follows 
#an approximate chi-squared distribution with 2-degrees of freedom

chi <- as.numeric(2* (logLik(my.po.2) - logLik(pglm)))
#look at page 110
#this is a very high p value which suggests that the 
#smaller model is a more likely to be the true model
pchisq(chi, 2, lower = FALSE)

pglm$coefficients
peta <- cbind(1, log(x)) %*% pglm$coefficients
myres <- y - peta
plot(peta, myres, ylab="residuals", xlab="fitted values")

I49 <- diag(49)
diag(I49) <- pglm$weights
W <- I49

x_star <- cbind(1, log(55))
X <- cbind(1, log(x))
CI_lower <- exp(x_star %*% pglm$coefficients 
          - 1.75*sqrt(t(x_star) %*% solve(t(X)%*% W %*% X) %*% x_star))

#use source("m3s2.R") to get function