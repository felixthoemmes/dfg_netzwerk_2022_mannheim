library(ipw)

#weighting example
#starting with simplest case
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

#ipw by hand
ps <- predict.glm(glm(X~V1+V2,family = "binomial"),type="response") #propensity score
num <- predict.glm(glm(X~1,family="binomial"),type="response") #this is just the mean
uw <- ifelse(X==1, 1/ps, 1/(1-ps)) #unstabilized weights
sw <- ifelse(X==1, num/ps, (1-num)/(1-ps)) #stabilized weights
lm(Y ~ X, weights=sw)

#using ipw package
ipw.s <- ipwpoint(
  exposure = X,
  family = "binomial",
  link="logit",
  numerator = ~ 1,
  denominator = ~ V1 + V2,
  data = df1)$ipw.weights

ipwplot(weights = ipw.s, logscale = FALSE)

#robust standard errors
f1 <- feols(Y ~ X, data = df1, weights=ipw.s, vcov = function(x) sandwich::vcovHC(x, type = "HC1")) 
summary(marginaleffects(f1)) #closely recovered at -.62

 
####time-varying####
#starting with simplest case of a time-varying treatment
set.seed(1234)
n <- 350

V1 <- rbinom(n,1,.5)
X1 <- exp(2*V1) /  (exp(2*V1) + 1)
X1 <- rbinom(n,1,X1)

V2 <- exp(0*X1) /  (exp(0*X1) + 1)
V2 <- rbinom(n,1,V2)

X2 <- exp(.3*V2 + .2*X1) /  (exp(.3*V2 + .2*X1) + 1)
X2 <- rbinom(n,1,X2)


Y <- exp(-2*V1 -.2*V2 + 0*X1 + 0*X2) /  (exp(-2*V1 -.3*V2 + 0*X1 + 0*X2) + 1)
Y <- rbinom(n,1,Y)


#true effect only through indirect effect of X1 on Y through V2
#all other direct effects of X1 and X2 set to zero
df1 <- data.frame(X1,X2,Y,V1,V2)

#unadjusted effect, contrast of interest might be treatment regime always 0 vs always 1
g1 <- glm(Y ~ X1 * X2, data=df1, 
          family=binomial(link = "logit"))
summary(emmeans(g1,specs=c("X1","X2"),contr="revpairwise",type="response"),
        infer=TRUE)#-1, biased


#weights at each timepoint, both stabilized
w1 <- ipwpoint(exposure = X1,
               family="binomial",
               link="logit",
               numerator = ~ 1,
               denominator = ~ V1,
               data=df1)$ipw.weights

w2 <- ipwpoint(exposure = X2,
               family="binomial",
               link="logit",
               numerator = ~ 1 + X1,
               denominator = ~ V1 + V2,
               data=df1)$ipw.weights


df1$fw <- w1*w2    

#adjusted model with weights
g2 <- glm(Y ~ X1 * X2, data=df1, weights=fw, 
          family=binomial(link = "logit"))
summary(emmeans(g2,specs=c("X1","X2"),contr="revpairwise", type="response"),infer=TRUE)
