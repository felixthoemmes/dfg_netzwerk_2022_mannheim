library(emmeans)
library(margins)
library(marginaleffects)
library(EffectLiteR)
library(MatchIt)

#regression example
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
df1 <- data.frame(X,Y,V1,V2)
#unadjusted regression
lm1 <- lm(Y~X) #effect 0

#linearly adjusted regression
lm2 <- lm(Y ~ X + V1 + V2) #effect-.5


#fully specified
lm3 <- lm(Y ~ X * (V1 + V2 + I(V1^2) + I(V2^2))) #effect .05

V1SQ <- V1^2
V2SQ <- V2^2

df2 <- data.frame(X,Y,V1,V2,V1SQ,V2SQ)

summary(margins(lm3,variables = "X"))
summary(emmeans(lm3,specs="X",contr="revpairwise"),infer = TRUE)
effectLite(y = "Y",x = "X",z = c("V1","V2","V1SQ","V2SQ"),data = df2)
summary(marginaleffects(lm3))

#treatment effect by hand using predictions
mean(predict(lm3,newdata = data.frame(X=rep(0,n)))) - 
  mean(predict(lm3,newdata = data.frame(X=rep(1,n))))

