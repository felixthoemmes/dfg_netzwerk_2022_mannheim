library(emmeans)
library(margins)
library(marginaleffects)
library(EffectLiteR)
library(MatchIt)
library(lmtest)
library(sandwich)
library(fixest)

#matching example
#just two variables but non-linear terms
set.seed(1234)
n <- 10000
U <- rnorm(n,0,1)
#two correlated confounders
V1 <- .3*U + rnorm(n,1,3)
V2 <- .3*U + rnorm(n,2,1)

X <- exp(.1*V1 - .3*V2) / 
  (exp(.1*V1 - .3*V2) + 1)
X <- rbinom(n,1,X)
Y <- .1*X + .2*V1 + .1*V1^2 + .2*V2 - .1*V2^2 + .3*X*V1 - .5*X*V2 + rnorm(n,4,1)
#true effect exactly -.6 (take partial deriative and input means)

#matching estimator
m1 <- matchit(X ~ V1 + V2, data = df1) #default 1:1 NN logistic reg
summary(m1) #balance stats

m1d <- match.data(m1)
f1 <- feols(Y ~ X, data = m1d, cluster~subclass, weights=m1d$weights)
summary(marginaleffects(f1))  #effect estimate -.32, very close to ATT from effectlite

m2 <- matchit(X ~ V1 + V2, data = df1, distance="bart", replace=TRUE)
summary(m2)
m2d <- match.data(m2)
f2 <- feols(Y ~ X, data = m2d, weights=m2d$weights, vcov = function(x) sandwich::vcovHC(x, type = "HC1")) #effect estimate -.32, very close to ATT from effectlite
summary(marginaleffects(f2))  #effect estimate -.27


m3 <- matchit(X ~ V1 + V2, data = df1, distance="cbps")
summary(m3)
m3d <- match.data(m3)
f3 <- feols(Y ~ X, data = m3d, weights=m3d$weights, cluster~subclass)
summary(marginaleffects(f3)) #effect estimate -.31, very close to ATT from effectlite

m4 <- matchit(X ~ V1 + V2, data = df1, method="cem", estimand="ATE")
m4d <- match.data(m4)
f4 <- feols(Y ~ X, data = m4d, weights=m4d$weights, cluster~subclass)
summary(marginaleffects(f4)) #effect estimate -.58, very close to ATT from effectlite
f4b <- feols(Y ~ X * (V1 + V2), data = m4d, weights=m4d$weights, cluster~subclass)
summary(marginaleffects(f4b)) #effect estimate -.60, very close to previous ATE
